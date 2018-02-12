{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.XML.Pugi hiding (getName, getValue)

import Poets.Critic.Parser
import Poets.Critic.Writer

main :: IO ()
main = do
    original <- parseGraph "fantasi-n2.xml"

    let originalGraph = generateGraph $ firstChild original

    case root original of
        Just x  -> do
            let r = generateGraph $ firstChild x
            case r of
                Nothing -> putStrLn "Failed"
                Just y  -> do
                            graph <- writeXML y
                            prettyFile (PrettyConfig "    " def def) "test.xml" graph
        Nothing -> putStrLn "Failed2"

    redone <- parseFile (ParseConfig parseFull encodingAuto) "test.xml"
    let redoneGraph = generateGraph $ firstChild redone

    case originalGraph of
        (Just x) -> case redoneGraph of
            (Just y) -> do
                if (x == y)
                    then putStrLn "graphs Equal"
                    else putStrLn "graphs NOT equal"
            (Nothing) -> putStrLn "Failed something"
        (Nothing) -> putStrLn "Failed something else"
    putStrLn "Done"
