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
    | (length $ getMessageTypeofID mts mtid) /= 1 = g
    | otherwise = Graph (xmlns g) newGt (graphInstance g)
  where
    m = Message n t
    gt = getGraphType g
    mts = getMessageTypes g
    mtWithID = head $ getMessageTypeofID mts mtid
    newMts = (delete mtWithID mts) ++ [MessageType mtid m]
    newGt = GraphType (graphTypeID gt) newMts (deviceTypes gt)

getMessageTypeofID :: [MessageType] -> String -> [MessageType]
getMessageTypeofID mts i = mtsWithID
  where
    mtsWithID = filter (\(MessageType n _) -> n == i) mts
