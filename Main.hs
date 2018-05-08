{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C (readFile, pack, unpack, ByteString)

import Poets.Critic
import Poets.Critic.Trees
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-td/fantasi-n2-td.xml"

    case original of
        Left  e  -> do
            putStrLn $ "Failed"
            putStrLn $ C.unpack e
        Right n  -> do
            let (Just g) = getGraph n
                deviceInstances = getDeviceInstances g
                tree = buildRandomTree deviceInstances
                algebra = getGraphAlgebra $ fst tree
                -- algebra = getGraphAlgebra $ getEdgeInstances g
            putStrLn algebra
            -- putStrLn $ show g
            putStrLn "Complete"
