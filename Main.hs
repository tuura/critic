{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
import Poets.Critic.Trees
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-random/fantasi-n1-random.xml"

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just g  -> do
            let deviceInstances = getDeviceInstances g
                tree = buildRandomTree deviceInstances
                -- algebra = getGraphAlgebra tree
                algebra = getGraphAlgebra $ getEdgeInstances g
            putStrLn algebra
