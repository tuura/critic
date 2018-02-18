{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic

main :: IO ()
main = do

    original <- parseFile "fantasi-n2.xml"

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just g  -> do -- printXMLtoFile y "test.xml"
            viewStats g
            putStrLn "\nAdding a device instance\n"
            let g1 = addDeviceInstance g "000088" "flipflop"
            viewStats g1
            putStrLn "\nAdding two edge instances for this new device instance\n"
            let g2 = addEdgeInstance g1 "000088:in-000032:out"
            let g3 = addEdgeInstance g2 "000056:in-000088:out"
            viewStats g3
