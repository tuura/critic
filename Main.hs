{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic
import Poets.Critic.Format

main :: IO ()
main = do

    original <- parseFile "test1.xml" --"fantasi-xmls/fantasi-n2.xml"

    case getGraph original of
        Nothing -> putStrLn "Failed"
        Just g  -> do
            viewStats g

            let g1 = formatDeviceInstanceIDs g

            viewPropertiesOfDeviceType g1 "flipflop"
            putStrLn "f000001 is: "
            viewPropertiesOfDeviceInstance g1 "f000001" -- Not parsing correctly so can't see anything.

            printXMLtoFile g1 "test.xml" -- This causes an error because of HEAD on an empty list


    -- TODO: Add property of "First"
    --       Add "rts = first" in  "__init__"
    --       Add output details to onReceive
    --       Generate random selection of first device.
    --       Allow option of first device
    --       Allow selection of file to be worked.
    --       Generate as binary to be used with tinsel/poets-ecosystem
    --       Add testing of this

