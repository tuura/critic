module Poets.Critic.Trees where

import Data.List

import Poets.Critic.Types

import System.Random

type Leaf = Bool

buildRandomTree :: [DeviceInstance] -> [EdgeInstance]
buildRandomTree ds = randomTree ds [] []

randomTree :: [DeviceInstance] -> [(DeviceInstance, Leaf)] -> [EdgeInstance] -> [EdgeInstance]
randomTree [] _ cs  = cs
randomTree ds [] cs = randomTree newDs [(d, False)] cs -- TODO: Set v as root!
  where
    d     = getRandomDevice ds
    newDs = delete d ds
randomTree ds vs cs = randomTree newDs (vs ++ [(d, True)]) (cs ++ c)
  where
    v     = getRandomDevice vs
    d     = getRandomDevice ds
    newVs = delete v vs ++ [(fst v, False)]
    newDs = delete d ds
    c     = connectDevice (fst v) d

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
