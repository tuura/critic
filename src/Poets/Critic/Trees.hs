module Poets.Critic.Trees where

import Data.List
import Data.Tuple.Utils

import Poets.Critic.Types

import System.Random

type Children = Int

type Level = Int

buildRandomTree :: [DeviceInstance] -> ([EdgeInstance], [DeviceInstance])
buildRandomTree ds = randomTree ds [] []

randomTree :: [DeviceInstance] -> [(DeviceInstance, Children, Level)] -> [EdgeInstance] -> ([EdgeInstance], [DeviceInstance])
randomTree [] vs cs  = (cs, addProperties vs)
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

addPropertyToDevI :: DeviceInstance -> DeviceProperty -> DeviceInstance
addPropertyToDevI d p = DeviceInstance devType insId ps
  where
    devType = deviceType d
    insId = deviceInstanceID d
    ps = deviceProperties d ++ [p]

addProperties :: [(DeviceInstance, Children, Level)] -> [DeviceInstance]
addProperties [] = []
addProperties (d:ds) = [setLevel (thd3 d) $ setChildren (snd3 d) (fst3 d)] ++ addProperties ds
  where
    setLevel 0 x = x
    setLevel l x = addPropertyToDevI x (DeviceProperty "level" (show l))
    setChildren 0 x = x
    setChildren c x = addPropertyToDevI x (DeviceProperty "children" (show c))
