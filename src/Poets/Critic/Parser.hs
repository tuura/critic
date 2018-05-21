module Poets.Critic.Parser (
  getFile, parseFile, getGraph
  ) where

import Data.ByteString.Char8 (unpack, ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List

import Xeno.DOM
import Xeno.Types

import Poets.Critic.Types hiding (getGraphInstance, getDeviceInstances,
                                  getEdgeInstances, getGraphType)

parseFile :: ByteString -> Either XenoException Node
parseFile r = parse r

getFile :: String -> IO ByteString
getFile p = BS.readFile p

getGraph :: Node -> Graph
getGraph n = Graph (getAttribute "xmlns" n) gt gi
  where
    gt = getGraphType cs
    gi = getGraphInstance cs gt
    cs = children n

getGraphType :: [Node] -> GraphType
getGraphType [] = GraphType [] [] []
getGraphType (n:ns)
    | (unpack $ name n) == "GraphType" = GraphType (getAttribute "id" n)
                                        (getMessages cs)
                                        (getDevices cs)
    | otherwise = getGraphType ns
  where
    cs = children n

getMessages :: [Node] -> [MessageType]
getMessages [] = []
getMessages (n:ns)
    | unpack nm == "MessageTypes" =
    concatMap (\c -> case (unpack $ name c) of
        "MessageType" -> [MessageType
                          (getAttribute "id" c)
                          (getMessageData (children c))]
        _             -> []
    ) cs
    | otherwise = getMessages ns
  where
    nm = name n
    cs = children n

getMessageData :: [Node] -> [Message]
getMessageData [] = []
getMessageData (n:ns) = map (\c ->
                      Message
                        (getChildName c)
                        (getChildType c)
                      ) cs ++ getMessageData ns
  where
    cs = children n
    getChildName c = getAttribute "name" c
    getChildType c = getAttribute "type" c

getDevices :: [Node] -> [DeviceType]
getDevices [] = []
getDevices (n:ns)
    | unpack nm == "DeviceTypes" =
          concatMap (\c -> case (unpack $ name c) of
              "DeviceType" -> [DeviceType
                  (getAttribute "id" c)
                  (getPropertyData $ getChildrenWithName "Properties" c)
                  (getStateData $ getChildrenWithName "State" c)
                  (inPs c)
                  (outPs c)
                  (rToS $ children c)
                  ] ++ (getDevices ns)
              _            -> []) (children n)
    | otherwise = getDevices ns
  where
    nm = name n
    inPs p = map (\i -> InputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (concatMap onRec (children i))
                    ) (getChildrenWithName "InputPin" p)
    onRec i = if (unpack $ name i) == "OnReceive"
        then getCData $ contents i
        else []
    outPs p = map (\i -> OutputPin
                        (getAttribute "name" i)
                        (getAttribute "messageTypeId" i)
                        (concatMap onSen (children i))
                     ) (getChildrenWithName "OutputPin" p)
    onSen o = if (unpack $ name o) == "OnSend"
        then getCData $ contents o
        else []
    rToS [] = []
    rToS (p:pins) = if (unpack $ name p) == "ReadyToSend"
        then getCData $ contents p
        else rToS pins

getStateData :: [Node] -> [State]
getStateData [] = []
getStateData (n:ns) = map (\c ->
                      State
                        (getChildName c)
                        (getChildType c)
                      ) cs ++ getStateData ns
  where
    cs = children n
    getChildName c = getAttribute "name" c
    getChildType c = getAttribute "type" c

getPropertyData :: [Node] -> [Property]
getPropertyData [] = []
getPropertyData (n:ns) = map (\c ->
                      Property
                        (getChildName c)
                        (getChildType c)
                        (getDefaultValue c)
                      ) cs ++ getPropertyData ns
  where
    cs = children n
    getChildName c = getAttribute "name" c
    getChildType c = getAttribute "type" c
    getDefaultValue c = getAttribute "default" c

getGraphInstance :: [Node] -> GraphType -> GraphInstance
getGraphInstance [] _ = GraphInstance [] [] [] []
getGraphInstance (n:ns) gt
    | unpack nm == "GraphInstance" = GraphInstance
                                         (getAttribute "graphTypeId" n)
                                         (getAttribute "id" n)
                                         dis eis
    | otherwise = getGraphInstance ns gt
  where
    nm = name n
    dis = getDeviceInstances (children n) (deviceTypes gt)
    eis = getEdgeInstances (children n) dis

getDeviceInstances :: [Node] -> [DeviceType] -> [DeviceInstance]
getDeviceInstances [] _ = []
getDeviceInstances (n:ns) dts
    | unpack nm == "DeviceInstances" = map (\c ->
                                         DeviceInstance
                                          (getAttribute "type" c)
                                          (getAttribute "id" c)
                                          (getDeviceProperties c)
                                         ) (children n)
    | otherwise = getDeviceInstances ns dts
  where
    nm = name n

getDeviceProperties :: Node -> [DeviceProperty]
getDeviceProperties d -- = [DeviceProperty (unpack $ name $ head cs) (show $ (unpack $ name $ head cs) == "P")]
    | cs == []  = []
    | otherwise = concatMap props ps
  where
    ps = getChildrenWithName "P" d
    cs = children d
    props p = concatMap (\x ->
                  DeviceProperty
                    (unpack $ fst x)
                    (unpack $ snd x)
                  ) (concatMap attributes cs)

getEdgeInstances :: [Node] -> [DeviceInstance] -> [EdgeInstance]
getEdgeInstances _ [] = []
getEdgeInstances [] _ = []
getEdgeInstances (n:ns) dis
    | unpack nm == "EdgeInstances" = map (\c -> parsePath c dis
                                         ) (children n)
    | otherwise = getEdgeInstances ns dis
  where
    nm = name n

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
getAttribute a n = isAttr atts
  where
    atts = attributes n
    isAttr [] = ""
    isAttr (x:xs) = if (unpack $ fst x) == a then (unpack $ snd x) else isAttr xs

getChildrenWithName :: String -> Node -> [Node]
getChildrenWithName s n = filter (\c -> (unpack $ name c) == s) cs
  where
    cs = children n

getCData :: [Content] -> String
getCData [] = []
getCData ((CData c):_) = unpack $ c
getCData (_:cs) = getCData cs
