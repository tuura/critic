module Poets.Critic.Parser (
    getGraph, parseFile
    ) where

import Data.ByteString.Char8 (pack, unpack)
import Data.List
import Data.List.Split

import Text.XML.Pugi hiding (getName, getValue, parseFile)
import qualified Text.XML.Pugi as Pugi

import Poets.Critic.Types hiding (getGraphInstance, getDeviceInstances,
                                  getEdgeInstances, getGraphType)

getGraph :: Document -> Maybe Graph
getGraph d = generateGraph $ firstChild d

parseFile :: String -> IO Document
parseFile p = Pugi.parseFile (ParseConfig parseFull encodingAuto) p

generateGraph :: Maybe Node -> Maybe Graph
generateGraph (Nothing) = Nothing
generateGraph (Just n)  = case getName n of
    "xml"     -> generateGraph $ nextSibling n
    "Graphs"  -> Just (Graph (getAttribute "xmlns" n) gt gi)
    _ -> Nothing
  where
    gt = getGraphType $ firstChild n
    gi = getGraphInstance (firstChild n) gt

getGraphType :: Maybe Node -> GraphType
getGraphType Nothing = GraphType "" [] []
getGraphType (Just n)
    | getName n == "GraphType" = GraphType (getAttribute "id" n)
                                           (getMessages $ firstChild n)
                                           (getDevices $ firstChild n)
    | otherwise = getGraphType $ nextSibling n

getMessages :: Maybe Node -> [MessageType]
getMessages Nothing  = []
getMessages (Just p)
    | getName p == "MessageTypes" =
    (concatMap (\n -> case getName n of
        "MessageType" -> [MessageType
                          (getAttribute "id" n)
                          (getMessage n)]
        _             -> []
    ) (listChildren $ p))
    | otherwise = getMessages $ nextSibling p
  where
    getMessage n = case firstChild n of
        (Just x) -> Message
                (getChildName (firstChild x))
                (getChildType (firstChild x))
        Nothing  -> Message "" ""
    getChildName (Just c)  = getAttribute "name" c
    getChildName (Nothing) = ""
    getChildType (Just c)  = getAttribute "type" c
    getChildType (Nothing) = ""

getDevices :: Maybe Node -> [DeviceType]
getDevices Nothing  = []
getDevices (Just p)
    | getName p == "DeviceTypes" =
          concatMap (\n -> case getName n of
              "DeviceType" -> [DeviceType
                  (getAttribute "id" n)
                  (ps n)
                  (sts n)
                  (inPs n)
                  (outPs n)
                  (rToS n)
                  ] ++ (getDevices $ nextSibling n)
              _            -> []) (listChildren p)
    | otherwise = getDevices $ nextSibling p
  where
    ps n = concatMap (\c ->
        [Property (getAttribute "type" c)
                  (getAttribute "name" c)
                  (getAttribute "default" c)]
        ) (getListOfAttribute "Properties" (firstChild n))
    sts n = concatMap (\c ->
        [State (getAttribute "name" c)
               (getAttribute "type" c)]
        ) (getListOfAttribute "State" (firstChild n))
    getInputs n = filter (\c -> (getName c) == "InputPin") (listChildren n)
    inPs n = map (\i -> InputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (onRec $ firstChild i)
                    ) (getInputs n)
    onRec (Just i) = if (getName i) == "OnReceive"
        then case firstChild $ i of
            Just x  -> getValue x
            Nothing -> ""
        else onRec $ nextSibling i
    onRec (Nothing) = ""
    getOutputs n = filter (\c -> (getName c) == "OutputPin") (listChildren n)
    outPs n = map (\i -> OutputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (onSen $ firstChild i)
                     ) (getOutputs n)
    onSen (Just o) = if (getName o) == "OnSend"
        then case firstChild o of
            Just x  -> getValue x
            Nothing -> ""
        else onSen $ nextSibling o
    onSen (Nothing) = ""
    rToS n = case firstChild $ head $ getRTS n of
        Just x  -> getValue x
        Nothing -> ""
    getRTS n = filter (\c -> (getName c) == "ReadyToSend") (listChildren n)

getListOfAttribute :: String -> Maybe Node -> [Node]
getListOfAttribute _ (Nothing) = []
getListOfAttribute s (Just n)
    | getName n == s = listChildren n
    | otherwise      = case nextSibling n of
      Just c  -> getListOfAttribute s (Just c)
      Nothing -> []

getGraphInstance :: Maybe Node -> GraphType -> GraphInstance
getGraphInstance Nothing _ = GraphInstance "" "" [] []
getGraphInstance (Just n) gt
    | getName n == "GraphInstance" = GraphInstance
                                         (getAttribute "graphTypeId" n)
                                         (getAttribute "id" n)
                                         dis eis
    | otherwise = getGraphInstance (nextSibling n) gt
  where
    dis = getDeviceInstances (firstChild n) (deviceTypes gt)
    eis = getEdgeInstances (firstChild n) dis

getDeviceInstances :: Maybe Node -> [DeviceType] -> [DeviceInstance]
getDeviceInstances Nothing _ = []
getDeviceInstances (Just n) dts
    | getName n == "DeviceInstances" = map (\c -> DeviceInstance
                                                      (di c)
                                                      (getAttribute "id" c)
                                                      (getDeviceProperties c dts)
                                           ) (listChildren n)
    | otherwise = getDeviceInstances (nextSibling n) dts
  where
    di c = getAttribute "type" c

getDeviceProperties :: Node -> [DeviceType] -> [DeviceProperty]
getDeviceProperties d dts
    | children == [] = []
    | otherwise      = parseProperties props
  where
    children = listChildren d
    p = head $ children
    props = trim $ getValue (head $ listChildren p)
    trim s = do
      let f = if (head s == ' ') then tail s else s
      let r = if (last f == ' ') then init f else f
      r

parseProperties :: String -> [DeviceProperty]
parseProperties props = map (\(f, s) -> DeviceProperty f s) form
  where
    sep = map (\s -> splitAt (ind s) s) (splitOn "," props)
    treat = map (\(p, v) -> ((tail $ init p), drop 2 v)) sep
    form = map (\(p, v) -> (p, v)) treat
    ind s = case (':' `elemIndex` s) of
      Just n  -> n
      Nothing -> 0

getEdgeInstances :: Maybe Node -> [DeviceInstance] -> [EdgeInstance]
getEdgeInstances Nothing _ = []
getEdgeInstances (Just n) dis
    | getName n == "EdgeInstances" = map (\c -> parsePath c dis
                                         ) (listChildren n)
    | otherwise = getEdgeInstances (nextSibling n) dis

parsePath :: Node -> [DeviceInstance] -> EdgeInstance
parsePath pth dis = EdgeInstance (i first) (o second)
  where
    p = getAttribute "path" pth
    first = elemIndex ':' p
    i (Just f) = findDeviceInstance (take f p) dis
    i Nothing = findDeviceInstance "" dis
    sp = elemIndex '-' p
    spl (Just s) = drop (s + 1) p
    spl (Nothing) = ""
    second = elemIndex ':' (spl sp)
    o (Just s) = findDeviceInstance (take s (spl sp)) dis
    o Nothing = findDeviceInstance "" dis

getAttribute :: String -> Node -> String
getAttribute att n = case Pugi.attribute (pack att) n of
    Just x  -> unpack x
    Nothing -> ""

getName :: Node -> String
getName n = unpack $ Pugi.getName n

getValue :: Node -> String
getValue n = unpack $ Pugi.getValue n

listChildren :: Node -> [Node]
listChildren = findChildren . firstChild
  where
    findChildren (Just x) = [x] ++ (findChildren $ nextSibling x)
    findChildren (Nothing) = []
