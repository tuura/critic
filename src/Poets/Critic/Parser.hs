{-# LANGUAGE OverloadedStrings #-}

module Poets.Critic.Parser (
    getGraph, parseFile
    ) where

import qualified Data.ByteString.Char8 as C (readFile, pack, unpack, ByteString)
import Data.List
import Data.List.Split
import Data.String.Utils

import Text.XML.Hexml hiding (attributeName, attributeValue)
import qualified Text.XML.Hexml as H (attributeValue)

import Poets.Critic.Types hiding (getGraphInstance, getDeviceInstances,
                                  getEdgeInstances, getGraphType)

getGraph :: Node -> Maybe Graph
getGraph d = generateGraph $ children d

parseFile :: String -> IO (Either C.ByteString Node)
parseFile p = do
  f <- C.readFile p
  return $ parse f

generateGraph :: [Node] -> Maybe Graph
generateGraph [] = Nothing
generateGraph (n:ns) = case name n of
    "xml"     -> generateGraph $ ns
    "Graphs"  -> Just (Graph (attributeValue $ attributeBy n "xmlns") gt gi)
    _ -> Nothing
  where
    gt = getGraphType $ children n
    gi = getGraphInstance (children n) gt

getGraphType :: [Node] -> GraphType
getGraphType [] = GraphType "" [] []
getGraphType (n:ns)
    | name n == "GraphType" = GraphType (attributeValue $ attributeBy n "id")
                                           (getMessages $ children n)
                                           (getDevices $ children n)
    | otherwise = getGraphType $ ns
  where

getMessages :: [Node] -> [MessageType]
getMessages []  = []
getMessages (p:ps)
    | name p == "MessageTypes" =
    (concatMap (\n -> case name n of
        "MessageType" -> [MessageType
                          (attributeValue $ attributeBy n "id")
                          (concatMap getMessage (children n))]
        _             -> []
    ) (children $ p))
    | otherwise = getMessages $ ps
  where
    getMessage n = map (\c -> Message
                (getChildName c)
                (getChildType c)) (children n)
    getChildName c = attributeValue $ attributeBy c "name"
    getChildType c = attributeValue $ attributeBy c "type"

getDevices :: [Node] -> [DeviceType]
getDevices [] = []
getDevices (p:ps)
    | name p == "DeviceTypes" =
          concatMap (\n -> case name n of
              "DeviceType" -> [DeviceType
                  (attributeValue $ attributeBy n "id")
                  (props n)
                  (sts n)
                  (inPs n)
                  (outPs n)
                  (rToS n)
                  ]
              _            -> []) (children p)
    | otherwise = getDevices ps
  where
    props n = concatMap (\c ->
        [Property (attributeValue $ attributeBy c "type")
                  (attributeValue $ attributeBy c "name")
                  (attributeValue $ attributeBy c "default")]
        ) (getListOfAttribute "Properties" (children n))
    sts n = concatMap (\c ->
        [State (attributeValue $ attributeBy c "name")
               (attributeValue $ attributeBy c "type")]
        ) (getListOfAttribute "State" (children n))
    getInputs n = filter (\c -> (name c) == "InputPin") (children n)
    inPs n = map (\i -> InputPin
                        (attributeValue $ attributeBy i "name")
                        (attributeValue $ attributeBy i "messageTypeId") ""
                        -- (concatMap onRec (children i))
                    ) (getInputs n)
    onRec i = C.unpack $ name i
    -- onRec i = if (name i) == "OnReceive"
    --     then if (length $ children i) /= 0
    --         then C.unpack $ inner (head $ children i)
    --         else ""
    --     else ""
    getOutputs n = filter (\c -> (name c) == "OutputPin") (children n)
    outPs n = map (\i -> OutputPin
                        (attributeValue $ attributeBy i "name")
                        (attributeValue $ attributeBy i "messageTypeId") ""
                        -- (concatMap onSen (children i))
                     ) (getOutputs n)
    onSen o = C.unpack $ name o
    -- onSen o = if (name o) == "OnSend"
    --     then if (length $ children o) /= 0
    --         then C.unpack $ inner (head $ children o)
    --         else ""
    --     else ""
    rToS n = C.unpack $ name n
    rToS n = if (length $ children $ head $ getRTS n) /= 0 -- TODO: Applying `head` hear makes me sad
        then C.unpack $ inner (head $ getRTS n)
        else ""
    getRTS n = filter (\c -> (name c) == "ReadyToSend") (children n)

getListOfAttribute :: String -> [Node] -> [Node]
getListOfAttribute _ []  = []
getListOfAttribute s (n:ns)
    | name n == C.pack s = children n
    | otherwise          = getListOfAttribute s ns

getGraphInstance :: [Node] -> GraphType -> GraphInstance
getGraphInstance [] _ = GraphInstance "" "" [] []
getGraphInstance (n:ns) gt
    | name n == "GraphInstance" =
        GraphInstance
            (attributeValue $ attributeBy n "graphTypeId")
            (attributeValue $ attributeBy n "id")
            dis
            eis
    | otherwise = getGraphInstance ns gt
  where
    dis = getDeviceInstances (children n) (deviceTypes gt)
    eis = getEdgeInstances (children n) dis

getDeviceInstances :: [Node] -> [DeviceType] -> [DeviceInstance]
getDeviceInstances [] _ = []
getDeviceInstances (n:ns) dts
    | name n == "DeviceInstances" = map (\c ->
                    DeviceInstance
                       (di c)
                       (attributeValue $ attributeBy c "id")
                       (getDeviceProperties c)
                                           ) (children n)
    | otherwise = getDeviceInstances (ns) dts
  where
    di c = attributeValue $ attributeBy c "type"

getDeviceProperties :: Node -> [DeviceProperty]
getDeviceProperties d
    | props == [] = []
    | otherwise  = parseProperties $ strip props
  where
    props = C.unpack $ inner d

parseProperties :: String -> [DeviceProperty]
parseProperties "" = []
parseProperties props = map (\(f, s) -> DeviceProperty f s) form
  where
    ps = replace "</P>" "" (replace "<P>" "" props) 
    sep = map (\s -> splitAt (ind s) s) (splitOn "," ps)
    treat = map (\(p, v) -> ((tail $ init p), drop 2 v)) sep
    form = map (\(p, v) -> (p, v)) treat
    ind s = case (':' `elemIndex` s) of
      Just n  -> n
      Nothing -> 0

getEdgeInstances :: [Node] -> [DeviceInstance] -> [EdgeInstance]
getEdgeInstances [] _ = []
getEdgeInstances (n:ns) dis
    | name n == "EdgeInstances" = map (\c -> parsePath c dis
                                         ) (children n)
    | otherwise = getEdgeInstances ns dis

parsePath :: Node -> [DeviceInstance] -> EdgeInstance
parsePath pth dis = EdgeInstance (i first) (o second)
  where
    p = attributeValue $ attributeBy pth (C.pack "path")
    first = elemIndex ':' p
    i (Just f) = findDeviceInstance (take f p) dis
    i Nothing = findDeviceInstance "" dis
    sp = elemIndex '-' p
    spl (Just s) = drop (s + 1) p
    spl (Nothing) = ""
    second = elemIndex ':' (spl sp)
    o (Just s) = findDeviceInstance (take s (spl sp)) dis
    o Nothing = findDeviceInstance "" dis

attributeValue :: Maybe Attribute -> String
attributeValue (Just a)  = C.unpack $ H.attributeValue $ a
attributeValue (Nothing) = ""
