module Poets.Critic.Trees where

import Data.List
import Data.Time.Clock

import Poets.Critic.Manipulate
import Poets.Critic.Types

import System.Random
import System.IO.Unsafe

data TreeNode = TreeNode
                {
                    devI :: DeviceInstance,
                    children :: Int,
                    level :: Int
                } deriving Eq

buildRandomTree :: [DeviceInstance] -> ([EdgeInstance], [DeviceInstance])
buildRandomTree ds = randomTree ds [] []

randomTree :: [DeviceInstance] -> [TreeNode] -> [EdgeInstance] -> ([EdgeInstance], [DeviceInstance])
randomTree [] vs cs = (cs, addDevIProperties vs)
randomTree ds [] cs = randomTree newDs [TreeNode newD 0 0] cs
  where
    d     = getRandomDevice ds
    newD  = addPropertyToDevI d (DeviceProperty "root" "1")
    newDs = delete d ds
randomTree ds vs cs = randomTree newDs newVs (cs ++ c)
  where
    v     = getRandomDevice vs
    d     = getRandomDevice ds
    newVs = delete v vs ++ [TreeNode (devI v) (children v + 1) (level v),
                            TreeNode d 0 (level v + 1)]
    newDs = delete d ds
    c     = connectDevices (devI v) d

buildBalancedTree :: [DeviceInstance] -> ([EdgeInstance], [DeviceInstance])
buildBalancedTree ds = buildTreeFromLevels dls [] []
  where
    root = getRandomDevice ds
    newDs = delete root ds
    dls = prepDeviceLevels newDs ++ [[root]]

prepDeviceLevels :: [DeviceInstance] -> [[DeviceInstance]]
prepDeviceLevels ds
    | n <= 3 = [ds]
    | length newDs == n = [thisLevel ++ [randDev]] ++ prepDeviceLevels newNewDs
    | otherwise =  [thisLevel] ++ prepDeviceLevels newDs
  where
    n = (length ds) `div` 2
    thisLevel = getNRandomDevices n ds
    newDs = ds \\ thisLevel
    randDev = getRandomDevice newDs
    newNewDs = delete randDev newDs

buildTreeFromLevels :: [[DeviceInstance]] -> [[TreeNode]] -> [EdgeInstance] -> ([EdgeInstance], [DeviceInstance])
buildTreeFromLevels [] vs cs = (cs, concatMap addDevIProperties vs)
buildTreeFromLevels (dl:dls) vs cs = buildTreeFromLevels dls newVs (cs ++ fst (connect vs))
  where
    levelSet = map (\d -> TreeNode d 0 (length dls)) dl
    connect [] = ([], levelSet)
    connect v = connectLevels (head v) levelSet
    botLevel = head vs
    newVs = if length levelSet /= 1 then [levelSet] ++ [snd $ connect vs] ++ delete botLevel vs
                                    else [snd $ connect vs] ++ delete botLevel vs

buildNTree :: [DeviceInstance] -> Int -> ([EdgeInstance], [DeviceInstance])
buildNTree ds n = buildTreeFromLevels dls [] []
  where
    root = getRandomDevice ds
    newDs = delete root ds
    dls = prepNDeviceLevels newDs n n ++ [[root]]

prepNDeviceLevels :: [DeviceInstance] -> Int -> Int -> [[DeviceInstance]]
prepNDeviceLevels [] _ _ = []
prepNDeviceLevels ds o n = prepNDeviceLevels newDs o (n * o) ++ [thisLevel]
  where
    thisLevel = getNRandomDevices n ds
    newDs = ds \\ thisLevel

getNRandomDevices :: Eq a => Int -> [a] -> [a]
getNRandomDevices _ [] = []
getNRandomDevices 0 _  = []
getNRandomDevices n ds = [d] ++ getNRandomDevices (n - 1) newDs
  where
    newDs = delete d ds
    d = getRandomDevice ds

connectLevels :: [TreeNode] -> [TreeNode] -> ([EdgeInstance], [TreeNode])
connectLevels [] _ = ([], [])
connectLevels bs ts  = connectLevels' bs ts []

connectLevels' :: [TreeNode] -> [TreeNode] -> [TreeNode] -> ([EdgeInstance], [TreeNode])
connectLevels' [] ts vs = ([], ts ++ vs)
connectLevels' bs [] vs = connectLevels' bs vs []
connectLevels' (b:bs) (t:ts) vs
    = (connectDevices (devI t) (devI b) ++ fst nxt, snd nxt)
  where
    newT = TreeNode (devI t) (children t + 1) (level t)
    nxt = connectLevels' bs ts (vs ++ [newT])

getRandomDevice :: [a] -> a
getRandomDevice ds = do
    let t = unsafePerformIO getCurrentTime
        time = fromInteger $ diffTimeToPicoseconds $ utctDayTime t
        n = length ds
        r = fst $ randomR (0, (n - 1)) (mkStdGen time)
    ds !! r

connectDevices :: DeviceInstance -> DeviceInstance -> [EdgeInstance]
connectDevices d1 d2 = [e1, e2]
  where
    e1 = EdgeInstance d1 d2
    e2 = EdgeInstance d2 d1

addDevIProperties :: [TreeNode] -> [DeviceInstance]
addDevIProperties [] = []
addDevIProperties ((TreeNode d c l):ds)
    = [isRoot $ setLevel $ setChildren d] ++ addDevIProperties ds
  where
    setLevel x = addPropertyToDevI x (DeviceProperty "level" (show l))
    setChildren x = addPropertyToDevI x (DeviceProperty "children" (show c))
    isRoot x = if l == 0 then addPropertyToDevI x (DeviceProperty "root" "1")
                         else x

countConnections :: DeviceInstance -> [EdgeInstance] -> Int
countConnections d es = length $ filter (\e -> comp e) es
  where
    devName = deviceInstanceID d
    edgeNameIn e = deviceInstanceID $ inNode e
    edgeNameOut e = deviceInstanceID $ outNode e
    comp e = (devName == edgeNameIn e) || (devName == edgeNameOut e)
