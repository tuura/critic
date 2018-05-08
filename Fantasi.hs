{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.ByteString.Char8 as C (readFile, pack, unpack, ByteString)
import System.Environment
import System.Random
import Poets.Critic
import Poets.Critic.Format
import Poets.Critic.Trees

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
            --Generate tree
            let (Just g) = getGraph n
                dis = getDeviceInstances g
                treeEdges = buildRandomTree dis
            putStrLn $ deviceStrings $ snd treeEdges
            putStrLn $ treeStrings $ fst treeEdges
            putStrLn "Complete"

deviceStrings :: [DeviceInstance] -> String
deviceStrings [] = ""
deviceStrings (d:ds)
    | props /= [] = str ++ ">\n\t\t\t<P>" ++ propStr ++ "</P>\n" ++ "\t\t</DevI>" ++ "\n" ++ deviceStrings ds
    | otherwise   = str ++ "/>" ++ "\n" ++ deviceStrings ds
  where
    props = deviceProperties d
    propStr = unwords $ intersperse ", " $ map (\p -> "\"" ++ deviceProperty p ++ "\": " ++ value p) props
    str = "\t\t<DevI type=\"" ++ deviceType d ++ "\" id=\"" ++ deviceInstanceID d ++ "\""

edgeStrings :: [EdgeInstance] -> String
edgeStrings [] = ""
edgeStrings (e:es) = "\t\t<EdgeI path=\"" ++ (deviceInstanceID $ inNode e) ++ ":in-" ++ (deviceInstanceID $ outNode e) ++ ":out\"/>\n" ++ edgeStrings es

treeStrings :: [EdgeInstance] -> String
treeStrings [] = ""
treeStrings (e:es) = "\t\t<EdgeI path=\"" ++ (deviceInstanceID $ inNode e) ++ ":tree_in-" ++ (deviceInstanceID $ outNode e) ++ ":tree_out\"/>\n" ++ treeStrings es
