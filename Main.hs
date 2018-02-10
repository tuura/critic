{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either

import Text.XML.Pugi hiding (getName, getValue)
import qualified Text.XML.Pugi as Pugi

import Types
import Writer

main = do
    original <- parseFile (ParseConfig parseFull encodingAuto) "poets-n2.xml"

    let originalGraph = generateGraph $ firstChild original

    case root original of
        Just x  -> do
            let r = generateGraph $ firstChild x
            -- putStrLn $ show r
            case r of
                Nothing -> putStrLn "Failed"
                Just y  -> do
                            -- putStrLn "Complete1"
                            graph <- writeXML y
                            -- putStrLn $ B.unpack $ pretty (PrettyConfig "    " def def) graph
                            prettyFile (PrettyConfig "    " def def) "text.xml" graph
        Nothing -> putStrLn "Failed2"

    redone <- parseFile (ParseConfig parseFull encodingAuto) "text.xml"
    let redoneGraph = generateGraph $ firstChild redone

    case originalGraph of
        (Just x) -> case redoneGraph of
            (Just y) -> do
                if (x == y)
                    then putStrLn "graphs Equal"
                    else putStrLn "graphs NOT equal"
            (Nothing) -> putStrLn "Failed something"
        (Nothing) -> putStrLn "Failed something else"
    putStrLn "Done"

generateGraph :: Maybe Node -> Maybe Graph
generateGraph (Nothing) = Nothing
generateGraph (Just n) = case getName n of
    "xml"    -> generateGraph $ nextSibling n
    "Graphs" -> Just (Graph (getAttribute "xmlns" n)
                            (getGraphType $ firstChild n)
                            (getGraphInstance $ firstChild n))
    otherwise -> Nothing
  where
    nameOf (Just x) = getName x
    nameOf (Nothing) = ""

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
        otherwise     -> []
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
                  (states n)
                  (inputPins n)
                  (outputPins n)
                  (readyToSend n)
                  ] ++ (getDevices $ nextSibling n)
              otherwise    -> []) (listChildren p)
    | otherwise = getDevices $ nextSibling p
  where
    getStates n = filter (\c -> (getName c) == "State") (listChildren n)
    states n = concatMap (\c -> case firstChild c of
        Just s -> [State
                    (getAttribute "name" s)
                    (getAttribute "type" s)]
        otherwise ->  []
                   ) (getStates n)
    getInputs n = filter (\c -> (getName c) == "InputPin") (listChildren n)
    inputPins n = map (\i -> InputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (onReceive $ firstChild i)
                    ) (getInputs n)
    onReceive (Just i) = if (getName i) == "OnReceive"
        then case firstChild $ i of
            Just x  -> getValue x
            Nothing -> ""
        else onReceive $ nextSibling i
    onReceive (Nothing) = ""
    getOutputs n = filter (\c -> (getName c) == "OutputPin") (listChildren n)
    outputPins n = map (\i -> OutputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (onSend $ firstChild i)
                     ) (getOutputs n)
    onSend (Just o) = if (getName o) == "OnSend"
        then case firstChild o of
            Just x  -> getValue x
            Nothing -> ""
        else onReceive $ nextSibling o
    onSend (Nothing) = ""
    readyToSend n = case firstChild $ head $ getRTS n of
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
