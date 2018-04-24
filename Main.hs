{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
import Poets.Critic.Trees
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-test/fantasi-n1-random.xml"

    case original of
        Left  e  -> putStrLn $ "Failed"
        Right n  -> do
            let (Just g) = getGraph n
                deviceInstances = getDeviceInstances g
                tree = buildRandomTree deviceInstances
                algebra = getGraphAlgebra tree
            putStrLn algebra
            putStrLn "Complete"


