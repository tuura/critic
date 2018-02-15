module Poets.Critic.Parser (
    getGraph, parseFile
    ) where

import Data.ByteString.Char8 (pack, unpack)

import Text.XML.Pugi hiding (getName, getValue, parseFile)
import qualified Text.XML.Pugi as Pugi

import Poets.Critic.Types

getGraph :: Document -> Maybe Graph
getGraph d = generateGraph $ firstChild d

parseFile :: String -> IO Document
parseFile p = Pugi.parseFile (ParseConfig parseFull encodingAuto) p

generateGraph :: Maybe Node -> Maybe Graph
generateGraph (Nothing) = Nothing
generateGraph (Just n)  = case getName n of
    "xml"     -> generateGraph $ nextSibling n
    "Graphs"  -> Just (Graph (getAttribute "xmlns" n)
                            (getGraphType $ firstChild n)
                            (getGraphInstance $ firstChild n))
    _ -> Nothing

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
                  (sts n)
                  (inPs n)
                  (outPs n)
                  (rToS n)
                  ] ++ (getDevices $ nextSibling n)
              _            -> []) (listChildren p)
    | otherwise = getDevices $ nextSibling p
  where
    getStates n = filter (\c -> (getName c) == "State") (listChildren n)
    sts n = concatMap (\c -> case firstChild c of
        Just s  -> [State
                    (getAttribute "name" s)
                    (getAttribute "type" s)]
        Nothing ->  []
                   ) (getStates n)
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

getGraphInstance :: Maybe Node -> GraphInstance
getGraphInstance Nothing = GraphInstance "" "" [] []
getGraphInstance (Just n)
    | getName n == "GraphInstance" = GraphInstance
                                         (getAttribute "graphTypeId" n)
                                         (getAttribute "id" n)
                                         (getDeviceInstances $ firstChild n)
                                         (getEdgeInstances $ firstChild n)
    | otherwise = getGraphInstance $ nextSibling n

getDeviceInstances :: Maybe Node -> [DeviceInstance]
getDeviceInstances Nothing = []
getDeviceInstances (Just n)
    | getName n == "DeviceInstances" = map (\c -> DeviceInstance
                                                      (getAttribute "type" c)
                                                      (getAttribute "id" c)
                                           ) (listChildren n)
    | otherwise = getDeviceInstances $ nextSibling n

getEdgeInstances :: Maybe Node -> [EdgeInstance]
getEdgeInstances Nothing = []
getEdgeInstances (Just n)
    | getName n == "EdgeInstances" = map (\c -> EdgeInstance
                                                    (getAttribute "path" c)
                                         ) (listChildren n)
    | otherwise = getEdgeInstances $ nextSibling n

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
