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
            let graph = getGraph n
            putStrLn "Success"
            printXML graph
            printXMLtoFile graph "test.xml"
