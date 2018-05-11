{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.ByteString.Char8 as C (readFile, pack, unpack, ByteString)
import System.Environment
import System.Random
import Poets.Critic
import Poets.Critic.Format
import Poets.Critic.Trees
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Error: Please provide one file path only"
        else prepareFile $ head args

prepareFile :: String -> IO ()
prepareFile path = do
    graph <- parseFile path

    case graph of
        Left  e  -> do
            putStrLn $ "Error: Graph cannot be parsed"
            putStrLn $ C.unpack e
        Right n  -> do
            let (Just g) = getGraph n
            --Count connections of standard connections
                dis = getDeviceInstances g
                eis = getEdgeInstances g
                con = map (\d -> (d, (countConnections d eis) `div` 2)) dis
                newDis = map (\(d, c) ->
                        addPropertyToDevI d
                        (DeviceProperty "connections" (show c))
                              ) con
            --Generate Random tree
                -- treeEdges = buildRandomTree newDis
            -- writeFile "newInfo.xml" $ treeStrings $ fst treeEdges
            -- putStrLn $ deviceStrings $ snd treeEdges
            -- putStrLn $ edgeStrings $ eis
            -- putStrLn $ treeStrings $ fst treeEdges
                -- algebra = getGraphAlgebra $ fst treeEdges
            -- putStrLn algebra

            -- Generate "Balanced" tree
                -- treeEdges = buildBalancedTree newDis
                -- algebra = getGraphAlgebra $ fst treeEdges
            -- putStrLn $ deviceStrings $ snd treeEdges
            -- putStrLn $ treeStrings $ fst treeEdges
            -- putStrLn algebra
            -- writeFile "newInfo.xml" $ treeStrings $ fst treeEdges
            -- writeFile "newInfo.xml" $ deviceStrings $ snd treeEdges

            -- Generate N tree
                treeEdges = buildNTree newDis 4
                algebra = getGraphAlgebra $ fst treeEdges

            -- putStrLn $ deviceStrings $ snd treeEdges
            -- putStrLn $ treeStrings $ fst treeEdges
            -- putStrLn $ algebra
            writeFile "newInfo.xml" $ (treeStrings $ fst treeEdges) ++ (deviceStrings $ snd treeEdges)
            -- writeFile "newInfo.xml" $ deviceStrings $ snd treeEdges
            putStrLn "Complete"

deviceStrings :: [DeviceInstance] -> String
deviceStrings [] = ""
deviceStrings (d:ds)
    | props /= [] = str ++ ">\n\t\t\t\t<P>" ++ propStr ++ "</P>\n" ++ "\t\t\t</DevI>" ++ "\n" ++ deviceStrings ds
    | otherwise   = str ++ "/>" ++ "\n" ++ deviceStrings ds
  where
    props = deviceProperties d
    propStr = unwords $ intersperse ", " $ map (\p -> "\"" ++ deviceProperty p ++ "\": " ++ value p) props
    str = "\t\t\t<DevI type=\"" ++ deviceType d ++ "\" id=\"" ++ deviceInstanceID d ++ "\""

edgeStrings :: [EdgeInstance] -> String
edgeStrings [] = ""
edgeStrings (e:es) = "\t\t\t<EdgeI path=\"" ++ (deviceInstanceID $ inNode e) ++ ":in-" ++ (deviceInstanceID $ outNode e) ++ ":out\"/>\n" ++ edgeStrings es

treeStrings :: [EdgeInstance] -> String
treeStrings [] = ""
treeStrings (e:es) = "\t\t\t<EdgeI path=\"" ++ (deviceInstanceID $ inNode e) ++ ":tree_in-" ++ (deviceInstanceID $ outNode e) ++ ":tree_out\"/>\n" ++ treeStrings es
