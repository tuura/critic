{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic.Parser
import Poets.Critic.Writer
import Poets.Critic.View

main :: IO ()
main = do

    original <- parseFile "fantasi-n2.xml"
    let originalGraph = getGraph original

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just y  -> printXMLtoFile y "test.xml"

    redone <- parseFile "test.xml"
    let redoneGraph = getGraph redone

    case originalGraph of
        (Just x) -> do
            viewMessageTypes x
            viewMessageTypesWithID x "updsate"
            -- viewStats x
            -- case redoneGraph of
            --     (Just y) -> do
            --         if (x == y)
            --             then putStrLn "graphs Equal"
            --             else putStrLn "graphs NOT equal"
            --     (Nothing) -> putStrLn "Failed something"
        (Nothing) -> putStrLn "Failed something else"



