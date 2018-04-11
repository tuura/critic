{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
import Poets.Critic.Format
import Poets.Graph.GraphAlgebra

main :: IO ()
main = do

    original <- parseFile "fantasi-xmls/fantasi-n2.xml"

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just g  -> do
            viewStats g
            -- putStrLn "\nAdding a device instance\n"
            -- let g1 = addDeviceInstance g "000088" "flipflop"
            -- viewStats g1
            -- viewDeviceInstances g1
            -- putStrLn "\nAdding two edge instances for this new device instance\n"
            -- let g2 = addEdgeInstance g1 "000088" "000032"
            -- let g3 = addEdgeInstance g2 "000056" "000088"
            -- viewStats g3
            let g1 = formatDeviceInstanceIDs g
            viewDeviceInstances g1
            viewEdgeInstances g1
            printXMLtoFile g1 "test.xml"
            -- printXML g3
            -- putStrLn "Output as graph algebra"
            -- putStrLn $ convertToAlgebra g
