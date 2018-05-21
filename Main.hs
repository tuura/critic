{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
-- import Poets.Critic.Trees
-- import Poets.Graph.GraphAlgebra

import Xeno.DOM
-- import Xeno.Types

main :: IO ()
main = do
    read <- getFile "fantasi-td/fantasi-n1-td.xml"
    let parsed = parseFile read

    case parsed of
        Left e  -> putStrLn $ show e
        Right n -> do
            putStrLn $ show $ getGraph n
            putStrLn "Success"
            -- let deviceInstances = getDeviceInstances g
                -- tree = buildRandomTree deviceInstances
                -- algebra = getGraphAlgebra tree
                -- algebra = getGraphAlgebra $ getEdgeInstances g
            -- putStrLn g

    -- case getGraph original of
    --     Nothing -> putStrLn "Failed"
    --     Just g  -> putStrLn "Success"
    --         -- let deviceInstances = getDeviceInstances g
    --             -- tree = buildRandomTree deviceInstances
    --             -- algebra = getGraphAlgebra tree
    --             -- algebra = getGraphAlgebra $ getEdgeInstances g
    --         -- putStrLn g
