module Poets.Critic.Format (
    formatDeviceInstanceIDs
    ) where

import Data.Char

import Poets.Critic.Types

-- | deviceInstanceIDs cannot start with a number. This will rename all
-- deviceInstanceIDs with the type initial, then the original number for all
-- DeviceInstance and EdgeInstance
formatDeviceInstanceIDs :: Graph -> Graph
formatDeviceInstanceIDs g = Graph (xmlns g) (graphType g) newGi
  where
    gi = getGraphInstance g
    dis = getDeviceInstances g
    eis = getEdgeInstances g
    newDis = map formatDeviceInstanceID dis
    newEis = map (\(EdgeInstance i o) ->
                        EdgeInstance (formatDeviceInstanceID i)
                                     (formatDeviceInstanceID o)) eis
    newGi = GraphInstance (instanceGraphTypeID gi)
                          (instanceID gi)
                          newDis
                          newEis

formatDeviceInstanceID :: DeviceInstance -> DeviceInstance
formatDeviceInstanceID (DeviceInstance t i)
    | isDigit $ head i = DeviceInstance t ([head $ deviceID t] ++ i)
    | otherwise        = DeviceInstance t i
