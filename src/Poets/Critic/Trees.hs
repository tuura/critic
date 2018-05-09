module Poets.Critic.Trees where

import Data.List
import Data.Tuple.Utils

import Poets.Critic.Manipulate
import Poets.Critic.Types

import System.Random

type Children = Int

type Level = Int

buildRandomTree :: [DeviceInstance] -> ([EdgeInstance], [DeviceInstance])
buildRandomTree ds = randomTree ds [] []

randomTree :: [DeviceInstance] -> [(DeviceInstance, Children, Level)] -> [EdgeInstance] -> ([EdgeInstance], [DeviceInstance])
randomTree [] vs cs = (cs, addProperties vs)
randomTree ds [] cs = randomTree newDs [(newD, 0, 0)] cs
  where
    d     = getRandomDevice ds
    newD  = addPropertyToDevI d (DeviceProperty "root" "1")
    newDs = delete d ds
randomTree ds vs cs = randomTree newDs (newVs) (cs ++ c)
  where
    v     = getRandomDevice vs
    d     = getRandomDevice ds
    newVs = delete v vs ++ [(fst3 v, (snd3 v + 1), thd3 v), (d, 0, thd3 v + 1)]
    newDs = delete d ds
    c     = connectDevice (fst3 v) d

getRandomDevice :: [a] -> a
getRandomDevice ds = do
    let r = fst $ randomR (0, (n - 1)) g
        x = ds !! r
    x
  where
    n = length ds
    g = mkStdGen n

connectDevice :: DeviceInstance -> DeviceInstance -> [EdgeInstance]
connectDevice d1 d2 = [e1, e2]
  where
    e1 = EdgeInstance d1 d2
    e2 = EdgeInstance d2 d1

addProperties :: [(DeviceInstance, Children, Level)] -> [DeviceInstance]
addProperties [] = []
addProperties (d:ds) = [setLevel (thd3 d) $ setChildren (snd3 d) (fst3 d)] ++ addProperties ds
  where
    setLevel l x = addPropertyToDevI x (DeviceProperty "level" (show l))
    setChildren c x = addPropertyToDevI x (DeviceProperty "children" (show c))

countConnections :: DeviceInstance -> [EdgeInstance] -> Int
countConnections d es = length $ filter (\e -> comp e) es
  where
    devName = deviceInstanceID d
    edgeNameIn e = deviceInstanceID $ inNode e
    edgeNameOut e = deviceInstanceID $ outNode e
    comp e = (devName == edgeNameIn e) || (devName == edgeNameOut e)
