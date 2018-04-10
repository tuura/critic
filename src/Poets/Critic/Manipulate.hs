module Poets.Critic.Manipulate (
    addMessageType, removeMessageType, addMessageToMessageType,
    removeMessageFromMessageType, addDeviceType, removeDeviceType,
    addStateToDeviceType, removeStateFromDeviceType,
    addInputPinToDeviceType, removeInputPinFromDeviceType,
    addOutputPinToDeviceType, removeOutputPinFromDeviceType,
    addReadyToSendToDeviceType, removeReadyToSendFromDeviceType,
    addDeviceInstance, removeDeviceInstance,
    addEdgeInstance, removeEdgeInstance
    ) where

import Poets.Critic.Types
import Data.List

-- Needs a check that the type doesn't already exist
addMessageType :: Graph -> String -> Graph
addMessageType g i = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    mts = getMessageTypes g
    newMts = mts ++ [MessageType i (Message "" "")]
    newGt = GraphType (graphTypeID gt) newMts (deviceTypes gt)

-- If the message type does not exist this returns the original graph
-- An error should be printed to this effect
removeMessageType :: Graph -> String -> Graph
removeMessageType g i
    | (length $ getMessageTypesOfID mts i) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    mts = getMessageTypes g
    rmMt = head $ getMessageTypesOfID mts i
    newMts = delete rmMt mts
    newGt = GraphType (graphTypeID gt) newMts (deviceTypes gt)

-- Needs a check that the message doesn't already exist
-- Currently returns the original graph if the MessageType doesn't exist
-- Needs to output an error when this occurs
addMessageToMessageType :: Graph -> String -> String -> String -> Graph
addMessageToMessageType g n t mtid
    | (length $ getMessageTypesOfID mts mtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    m = Message n t
    gt = getGraphType g
    mts = getMessageTypes g
    mtWithID = head $ getMessageTypesOfID mts mtid
    newMts = (delete mtWithID mts) ++ [MessageType mtid m]
    newGt = GraphType (graphTypeID gt) newMts (deviceTypes gt)

-- If message type or message does not exist, this returns the original graph
-- An error needs to be printed explaining so to the user
removeMessageFromMessageType :: Graph -> String -> Graph
removeMessageFromMessageType g mtid
    | (length $ getMessageTypesOfID mts mtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    m = Message "" ""
    gt = getGraphType g
    mts = getMessageTypes g
    mtWithID = head $ getMessageTypesOfID mts mtid
    newMts = (delete mtWithID mts) ++ [MessageType (messageID mtWithID) m]
    newGt = GraphType (graphTypeID gt) newMts (deviceTypes gt)

getMessageTypesOfID :: [MessageType] -> String -> [MessageType]
getMessageTypesOfID mts i = mtsWithID
  where
    mtsWithID = filter (\(MessageType n _) -> n == i) mts

-- Needs a check that the type doesn't already exist
addDeviceType :: Graph -> String -> Graph
addDeviceType g i = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    dts = getDeviceTypes g
    newDts = dts ++ [DeviceType i [] [] [] []]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- Currently returns the original graph if no device type is found
-- An error message should be printed instead
removeDeviceType :: Graph -> String -> Graph
removeDeviceType g i
    | (length $ getDeviceTypesOfID dts i) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    dts = getDeviceTypes g
    newDts = (delete rmDt dts)
    rmDt = head $ getDeviceTypesOfID dts i
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- Needs a check that the device type doesn't already exist
-- Currently returns the original graph if the DeviceType doesn't exist
-- Needs to output an error when this occurs
addStateToDeviceType :: Graph -> String -> String -> String -> Graph
addStateToDeviceType g n t dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    s = State n t
    sts = states dtWithID
    newSts = sts ++ [s]
    gt = getGraphType g
    dts = getDeviceTypes g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType i _ ins outs rts) = DeviceType i newSts ins outs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- If the device or the state doesn't exist, returns original graph
-- Needs to output an error when this occurs
removeStateFromDeviceType :: Graph -> String -> String -> Graph
removeStateFromDeviceType g n dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | (length $ getStatesWithName sts n) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    dts = getDeviceTypes g
    gt = getGraphType g
    sts = states dtWithID
    newSts = delete rmSt sts
    rmSt = head $ getStatesWithName sts n
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType i _ ins outs rts) = DeviceType i newSts ins outs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

getStatesWithName :: [State] -> String -> [State]
getStatesWithName sts n = stsWithID
  where
    stsWithID = filter (\(State x _) -> x == n) sts

-- Needs a check that the input pin doesn't already exist
-- Currently returns the original graph if the DeviceType doesn't exist
-- Needs to output an error when this occurs
addInputPinToDeviceType :: Graph -> String -> String -> String -> String -> Graph
addInputPinToDeviceType g n mtid r dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    i = InputPin n mtid r
    is = inputPins dtWithID
    newIs = is ++ [i]
    gt = getGraphType g
    dts = getDeviceTypes g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType nm ss _ outs rts) = DeviceType nm ss newIs outs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- If the device type or input pin don't exist this returns the original graph
-- An error needs to be returned in this case
removeInputPinFromDeviceType :: Graph -> String -> String -> Graph
removeInputPinFromDeviceType g n dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | (length $ getInputPinsWithName is n) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    is = inputPins dtWithID
    dts = getDeviceTypes g
    gt = getGraphType g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    iWithID = head $ getInputPinsWithName is n
    newIs = delete iWithID is
    newDt (DeviceType nm ss _ outs rts) = DeviceType nm ss newIs outs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

getInputPinsWithName :: [InputPin] -> String -> [InputPin]
getInputPinsWithName is n = isWithID
  where
    isWithID = filter (\(InputPin x _ _) -> x == n) is

-- Needs a check that the output pin doesn't already exist
-- Currently returns the original graph if the DeviceType doesn't exist
-- Needs to output an error when this occurs
addOutputPinToDeviceType :: Graph -> String -> String -> String -> String -> Graph
addOutputPinToDeviceType g n mtid r dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    o = OutputPin n mtid r
    os = outputPins dtWithID
    newOs = os ++ [o]
    gt = getGraphType g
    dts = getDeviceTypes g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType nm ss ins _ rts) = DeviceType nm ss ins newOs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- If the device type or output pin don't exist this returns the original graph
-- An error needs to be returned in this case
removeOutputPinFromDeviceType :: Graph -> String -> String -> Graph
removeOutputPinFromDeviceType g n dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | (length $ getOutputPinsWithName os n) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    os = outputPins dtWithID
    dts = getDeviceTypes g
    gt = getGraphType g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    oWithID = head $ getOutputPinsWithName os n
    newOs = delete oWithID os
    newDt (DeviceType nm ss ins _ rts) = DeviceType nm ss ins newOs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

getOutputPinsWithName :: [OutputPin] -> String -> [OutputPin]
getOutputPinsWithName os n = osWithID
  where
    osWithID = filter (\(OutputPin x _ _) -> x == n) os

-- Needs a check that the ready to send of this device doesn't already exist
-- Currently returns the original graph if the DeviceType doesn't exist
-- Needs to output an error when this occurs
addReadyToSendToDeviceType :: Graph -> String -> String -> Graph
addReadyToSendToDeviceType g rts dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    dts = getDeviceTypes g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType nm ss ins outs _) = DeviceType nm ss ins outs rts
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

-- If the device type doesn't exist, returns the original graph
-- Needs an error message to be output if this happens
removeReadyToSendFromDeviceType :: Graph -> String -> Graph
removeReadyToSendFromDeviceType g dtid
    | (length $ getDeviceTypesOfID dts dtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    gt = getGraphType g
    dts = getDeviceTypes g
    dtWithID = head $ getDeviceTypesOfID dts dtid
    newDt (DeviceType nm ss ins outs _) = DeviceType nm ss ins outs ""
    newDts = (delete dtWithID dts) ++ [newDt dtWithID]
    newGt = GraphType (graphTypeID gt) (messageTypes gt) newDts

getDeviceTypesOfID :: [DeviceType] -> String -> [DeviceType]
getDeviceTypesOfID dts i = dtsWithID
  where
    dtsWithID = filter (\(DeviceType n _ _ _ _) -> n == i) dts

-- Needs a check that the device type given exists
-- Needs a check taht the device ID is not already taken
-- Needs an output for when this occurs
addDeviceInstance :: Graph -> String -> String -> Graph
addDeviceInstance g i t = Graph (xmlns g) (graphType g) (newGi gi)
  where
    gi = getGraphInstance g
    dis = getDeviceInstances g
    dts = getDeviceTypes g
    newDt = findDeviceType t dts
    newDi = DeviceInstance newDt i
    newDis = dis ++ [newDi]
    newGi (GraphInstance m n _ e) = GraphInstance m n newDis e

-- If the device instance does not exist, the original graph is returned
-- An error should be sent informing this has occurred
removeDeviceInstance :: Graph -> String -> Graph
removeDeviceInstance g i
    | (length $ getDeviceInstancesWithID dis i) /= 1 = g
    | otherwise = Graph (xmlns g) (graphType g) (newGi gi)
  where
    gi = getGraphInstance g
    dis = getDeviceInstances g
    diWithID = head $ getDeviceInstancesWithID dis i
    newDis = delete diWithID dis
    newGi (GraphInstance m n _ e) = GraphInstance m n newDis e

getDeviceInstancesWithID :: [DeviceInstance] -> String -> [DeviceInstance]
getDeviceInstancesWithID dis i = disWithID
  where
    disWithID = filter (\(DeviceInstance _ n) -> n == i) dis

-- Needs a check that the path does not already exist
-- Paths could possibly be parsed further, separating into in and out
addEdgeInstance :: Graph -> String -> String -> Graph
addEdgeInstance g i o = Graph (xmlns g) (graphType g) (newGi gi)
  where
    gi = getGraphInstance g
    eis = getEdgeInstances g
    dis = getDeviceInstances g
    newEi = EdgeInstance newIn newOut
    newEis = eis ++ [newEi]
    newGi (GraphInstance m n d _) = GraphInstance m n d newEis
    newIn  = findDeviceInstance i dis
    newOut = findDeviceInstance o dis

removeEdgeInstance :: Graph -> String -> Graph
removeEdgeInstance g p
    | (length $ getEdgeInstancesWithPath eis p) /= 1 = g
    | otherwise = Graph (xmlns g) (graphType g) (newGi gi)
  where
    gi = getGraphInstance g
    eis = getEdgeInstances g
    eiWithID = head $ getEdgeInstancesWithPath eis p
    newEis = delete eiWithID eis
    newGi (GraphInstance m n d _) = GraphInstance m n  d newEis

getEdgeInstancesWithPath :: [EdgeInstance] -> String -> [EdgeInstance]
getEdgeInstancesWithPath eis p = usefulEis
  where
    eisWithPath = filter (== p) asPaths
    is = elemIndices p eisWithPath
    usefulEis = map (eis !!) is
    asPaths = map (\e -> (deviceInstanceID $ inNode e) ++ ":in-"
               ++ (deviceInstanceID $ outNode e) ++ ":out\n")
                eis

-- parsePath :: String -> [DeviceInstance] -> EdgeInstance
-- parsePath p dis = EdgeInstance i o
--   where
--     (Just first) = elemIndex ':' p
--     i = findDeviceInstance (take first p) dis
--     (Just sp) = elemIndex '-' p
--     split = drop (sp + 1) p
--     (Just second) = elemIndex ':' split
--     o = findDeviceInstance (take second split) dis
