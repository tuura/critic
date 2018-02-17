module Poets.Critic.Manipulate where

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

getDeviceTypesOfID :: [DeviceType] -> String -> [DeviceType]
getDeviceTypesOfID dts i = dtsWithID
  where
    dtsWithID = filter (\(DeviceType n _ _ _ _) -> n == i) dts
