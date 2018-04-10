{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
import Poets.Formats.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-n2.xml"

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just g  -> do
            -- viewStats g
            -- putStrLn "\nAdding a device instance\n"
            -- let g1 = addDeviceInstance g "000088" "flipflop"
            -- viewStats g1
            -- putStrLn "\nAdding two edge instances for this new device instance\n"
            -- let g2 = addEdgeInstance g1 "000088" "000032"
            -- let g3 = addEdgeInstance g2 "000056" "000088"
            -- viewStats g3
            -- viewEdgeInstances g3
            -- printXMLtoFile g3 "test.xml"
            putStrLn "Output in graph algebra"
            putStrLn $ convertToAlgebra g
