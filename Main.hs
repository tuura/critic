{-# LANGUAGE OverloadedStrings #-}

module Main where

import Poets.Critic

main :: IO ()
main = do
    read <- getFile "fantasi-n-tree/fantasi-n1-2-tree.xml"
    let parsed = parseFile read
    case parsed of
        Left e  -> putStrLn $ show e
        Right n -> do
            let graph = getGraph n
            putStrLn "Success"
            printXML graph
            printXMLtoFile graph "test.xml"
