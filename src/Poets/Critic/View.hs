module Poets.Critic.View (
    viewStats, viewxmlns, viewGraphTypeID,
    viewGraphInstance, viewMessageTypes,
    viewMessageTypesWithID, viewMessages,
    viewDeviceTypes, viewDeviceTypesWithID,
    viewPropertiesOfDeviceType,
    viewStatesOfDeviceType, viewInputPinsOfDeviceType,
    viewOnReceiveOfInputPin,
    viewOutputPinsOfDeviceType, viewOnSendOfOutputPin,
    viewReadyToSendOfDeviceType,
    viewDeviceInstances, viewDeviceInstancesOfType,
    viewPropertiesOfDeviceInstance,
    viewEdgeInstances, viewDeviceInstanceWithID
    ) where

import Poets.Critic.Types

viewStats :: Graph -> IO ()
viewStats g = do
    putStrLn $ "xmlns: " ++ xmlns g
    let gt = graphType g
    putStrLn $ "Graph type id             : " ++ graphTypeID gt
    let lm = length $ messageTypes gt
    putStrLn $ "Number of message types   : " ++ show lm
    let ld = length $ deviceTypes gt
    putStrLn $ "Number of device types    : " ++ show ld
    let gi = graphInstance g
    putStrLn $ "Graph instance type       : " ++ instanceGraphTypeID gi
    putStrLn $ "Graph instance ID         : " ++ instanceID gi
    let ldi = length $ deviceInstances gi
    putStrLn $ "Number of device instances: " ++ show ldi
    let lei = length $ edgeInstances gi
    putStrLn $ "Number of edge instances  : " ++ show lei

viewxmlns :: Graph -> IO ()
viewxmlns g = putStrLn $ "xmlns: " ++ xmlns g

viewGraphTypeID :: Graph -> IO ()
viewGraphTypeID g = putStrLn $ "Graph type ID: " ++ graphTypeID gt
  where
    gt = graphType g

viewMessageTypes :: Graph -> IO ()
viewMessageTypes g = putStrLn $ unlines $ getMessageTypesInfo mts
  where
    mts = getMessageTypes g

getMessageTypesInfo :: [MessageType] -> [String]
getMessageTypesInfo mts =
    concatMap (\mt -> ["Message type ID: " ++ messageID mt]
                  ++ [unlines $ map getMessageInfo (messages mt)]) mts

getMessageInfo :: Message -> String
getMessageInfo (Message n t)
    | n == "" && t == "" = "No name or type\n"
    | otherwise          = "Message name: " ++ n ++ "\n"
                        ++ "Message type: " ++ t ++ "\n"

viewMessageTypesWithID :: Graph -> String -> IO ()
viewMessageTypesWithID g i
    | mtsWithID == [] = putStrLn $
                        "Error: No messages with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getMessageTypesInfo mtsWithID
  where
    mts = getMessageTypes g
    mtsWithID = filter (\(MessageType n _) -> n == i) mts

viewMessages :: Graph -> IO ()
viewMessages = viewMessageTypes

viewDeviceTypes :: Graph -> IO ()
viewDeviceTypes g = putStrLn $ unlines $ getDeviceTypesInfo dts
  where
    dts = getDeviceTypes g

viewDeviceTypesWithID :: Graph -> String -> IO ()
viewDeviceTypesWithID g i
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getDeviceTypesInfo dtsWithID
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == i) dts

getDeviceTypesInfo :: [DeviceType] -> [String]
getDeviceTypesInfo dts =
    concatMap (\dt -> ["Device type ID : " ++ deviceID dt]
                   ++ (getStatesInfo $ states dt)
                   ++ (getInputPinsInfo $ inputPins dt)
                   ++ (getOutputPinsInfo $ outputPins dt)
                   ++ ["Code run when ready to send: " ++ readyToSend dt]) dts

getPropertiesInfo :: [Property] -> [String]
getPropertiesInfo ps =
    concatMap (\p -> ["Property type   : " ++ propertyType p]
                  ++ ["Property name   : " ++ propertyName p]
                  ++ ["Property default: " ++ propertyDefault p]) ps

getStatesInfo :: [State] -> [String]
getStatesInfo sts =
    concatMap (\st -> ["State name: " ++ stateName st]
                   ++ ["State type: " ++ stateType st]) sts

getInputPinsInfo :: [InputPin] -> [String]
getInputPinsInfo is =
    concatMap (\i -> ["Input pin name        : " ++ inputName i]
                  ++ ["Input pin message type: " ++ iMessageTypeID i]
                  ++ ["Code run on receive   : " ++ onReceive i]) is

getOutputPinsInfo :: [OutputPin] -> [String]
getOutputPinsInfo os =
    concatMap (\o -> ["Output pin name        : " ++ outputName o]
                  ++ ["Output pin message type: " ++ oMessageTypeID o]
                  ++ ["Code run on send       : " ++ onSend o]) os

viewPropertiesOfDeviceType :: Graph -> String -> IO ()
viewPropertiesOfDeviceType g i
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getPropertiesInfo psOfDts
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == i) dts
    psOfDts = concatMap (properties) dtsWithID

viewStatesOfDeviceType :: Graph -> String -> IO ()
viewStatesOfDeviceType g i
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getStatesInfo stsOfDts
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == i) dts
    stsOfDts = concatMap (states) dtsWithID

viewInputPinsOfDeviceType :: Graph -> String -> IO ()
viewInputPinsOfDeviceType g i
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getInputPinsInfo ipsOfDts
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == i) dts
    ipsOfDts = concatMap (inputPins) dtsWithID

viewOnReceiveOfInputPin :: Graph -> String -> IO ()
viewOnReceiveOfInputPin g n
    | isWithID == [] = putStrLn $
                       "Error: No input pins with ID \"" ++ n ++ "\" found."
    | otherwise = putStrLn $ unlines $ getInputPinsInfo isWithID
  where
    dts = getDeviceTypes g
    isWithID = filter (\i -> inputName i == n) allIs
    allIs = concatMap inputPins dts

viewOutputPinsOfDeviceType :: Graph -> String -> IO ()
viewOutputPinsOfDeviceType g o
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ o ++ "\" found."
    | otherwise = putStrLn $ unlines $ getOutputPinsInfo opsOfDts
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == o) dts
    opsOfDts = concatMap (outputPins) dtsWithID

viewOnSendOfOutputPin :: Graph -> String -> IO ()
viewOnSendOfOutputPin g n
    | osWithID == [] = putStrLn $
                       "Error: No output pins with ID \"" ++ n ++ "\" found."
    | otherwise = putStrLn $ unlines $ getOutputPinsInfo osWithID
  where
    dts = getDeviceTypes g
    osWithID = filter (\o -> outputName o == n) allOs
    allOs = concatMap outputPins dts

viewReadyToSendOfDeviceType :: Graph -> String -> IO ()
viewReadyToSendOfDeviceType g i
    | dtsWithID == [] = putStrLn $
                        "Error: No devices with ID \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $
                  map (\r -> "Code run when ready to send: " ++ r) rtsOfDts
  where
    dts = getDeviceTypes g
    dtsWithID = filter (\(DeviceType n _ _ _ _ _) -> n == i) dts
    rtsOfDts = map (readyToSend) dtsWithID

viewGraphInstance :: Graph -> IO ()
viewGraphInstance g = putStrLn $ unlines $ getGraphInstanceInfo gi
  where
    gi = getGraphInstance g

getGraphInstanceInfo :: GraphInstance -> [String]
getGraphInstanceInfo gi =
        ["Graph type ID             : " ++ instanceGraphTypeID gi]
     ++ ["Graph instance ID         : " ++ instanceID gi]
     ++ ["Number of device instances: " ++ show ldi]
     ++ ["Number of edge instances  : " ++ show lei]
  where
    ldi = length $ deviceInstances gi
    lei = length $ edgeInstances gi

viewDeviceInstances :: Graph -> IO ()
viewDeviceInstances g = putStrLn $ unlines $ getDeviceInstancesInfo dis
  where
    dis = getDeviceInstances g

viewDeviceInstancesOfType :: Graph -> String -> IO ()
viewDeviceInstancesOfType g t
    | disOfType == [] = putStrLn $
                        "Error: No devices of type \"" ++ t ++ "\" found."
    | otherwise = putStrLn $ unlines $ getDeviceInstancesInfo disOfType
  where
    dis = getDeviceInstances g
    disOfType = filter (\(DeviceInstance n _ _) -> n == t) dis

viewDeviceInstanceWithID :: Graph -> String -> IO ()
viewDeviceInstanceWithID g i
    | disWithID == [] = putStrLn $
                        "Error: No device with id \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ unlines $ getDeviceInstancesInfo disWithID
  where
    dis = getDeviceInstances g
    disWithID = filter (\(DeviceInstance _ n _) -> n == i) dis

viewPropertiesOfDeviceInstance :: Graph -> String -> IO ()
viewPropertiesOfDeviceInstance g i
    | disWithID == [] = putStrLn $
                        "Error: No device with id \"" ++ i ++ "\" found."
    | otherwise = putStrLn $ getDevicePropertiesInfo $ head disWithID
  where
    dis = getDeviceInstances g
    disWithID = filter (\(DeviceInstance _ n _) -> n == i) dis

getDeviceInstancesInfo :: [DeviceInstance] -> [String]
getDeviceInstancesInfo dis =
    map (\di -> "Device ID: " ++ deviceInstanceID di ++ "\n" ++
                " Device type: " ++ (deviceType di) ++ "\n" ++
                " Device properties: " ++ getDevicePropertiesInfo di) dis

getDevicePropertiesInfo :: DeviceInstance -> String
getDevicePropertiesInfo di =
    concatMap (\(DeviceProperty p v) -> "Property: " ++ p ++ "\n"
                                     ++ "Value: " ++ v) (deviceProperties di)

viewEdgeInstances :: Graph -> IO ()
viewEdgeInstances g = putStrLn $ unlines $ getEdgeInstancesInfo eis
  where
    eis = getEdgeInstances g

getEdgeInstancesInfo :: [EdgeInstance] -> [String]
getEdgeInstancesInfo eis =
    map (\ei -> "Path: " ++ (deviceInstanceID $ inNode ei) ++ ":in-"
                         ++ (deviceInstanceID $ outNode ei) ++ ":out") eis
