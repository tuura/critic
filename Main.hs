{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C (readFile, pack, unpack, ByteString)

import Poets.Critic
import Poets.Critic.Trees
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-xmls/fantasi-n1.xml"

    case original of
        Left  e  -> do
            putStrLn $ "Failed"
            putStrLn $ C.unpack e
        Right n  -> do
            case getGraph n of
                Just g -> do
                    let deviceInstances = getDeviceInstances g
                        tree = buildRandomTree deviceInstances
                        algebra = getGraphAlgebra $ fst tree
                        -- algebra = getGraphAlgebra $ getEdgeInstances g
                    putStrLn algebra
                    -- putStrLn $ show g
                    putStrLn "Complete"
                otherwise -> putStrLn "Fail"
