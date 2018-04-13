{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import System.Environment
import System.Random
import Poets.Critic
import Poets.Critic.Format

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Error: Please provide one file path only"
        else prepareFile $ head args

prepareFile :: String -> IO ()
prepareFile path = do
    graph <- parseFile path

    case getGraph graph of
        Nothing -> putStrLn "Error: Graph cannot be parsed"
        Just g  -> do
        -- Add "first" property
          let g1 = addPropertyToDeviceType g "uint8_t" "first" "0" "flipflop"
        -- Add line of initialisation for choosing the first device
              g2 = addToOnReceiveOfInputPinOfDeviceType g1
                      "__init__" "flipflop"
                      "deviceState->rts = deviceProperties->first;"
        -- Add handler_log info for printing distance number to console.
              g3 = replaceOnReceiveOfInputPinOfDeviceType g2
                     "in"
                     "flipflop"
                    ("\n\t\t\t\t\t\t" ++ "if (message->distance < deviceState->distance){" ++ "\n\t"
                  ++ "\t\t\t\t\t\t" ++ "deviceState->distance = message->distance;" ++ "\n\t"
                  ++ "\t\t\t\t\t\t" ++ "deviceState->rts = 1;" ++ "\n\t"
                  ++ "\t\t\t\t\t\t" ++ "handler_log(3, \"distance: %d\", deviceState->distance);"
                  ++ "\t\t\t\t\t\t" ++ "\n"
                  ++ "\t\t\t\t\t\t" ++ "}\n")

              g4 = formatDeviceInstanceIDs g3
        -- Generate random number to represent the first device
              noOfFlipflops = length $ getDeviceInstances g
          randomNum <- randomRIO (0, (noOfFlipflops - 1))
          putStrLn $ show randomNum
        -- Get first flipflop
          let flipflops = getDeviceInstancesOfType g4 $
                        findDeviceType "flipflop" $
                        getDeviceTypes g4
              firstFF = flipflops !! randomNum

          putStrLn $ deviceInstanceID firstFF

          let g5 = addDeviceInstanceProperty g4 (deviceInstanceID $ firstFF) "first" "1"

          writeFile "devs.xml" (edgeStrings $ getEdgeInstances g5)

          -- printXMLtoFile g5 "fantasi-test.xml" -- This causes an error because of HEAD on an empty list

          putStrLn "Complete"

    -- TODO: Add property to a device instance
    --       Generate random selection of first device.
    --       Allow option of first device
    --       Generate as binary to be used with tinsel/poets-ecosystem
    --       Add testing of this

deviceStrings :: [DeviceInstance] -> String
deviceStrings [] = ""
deviceStrings (d:ds)
    | props /= [] = str ++ ">\n\t<P>" ++ propStr ++ "</DevI>" ++ "\n" ++ deviceStrings ds
    | otherwise   = str ++ "/>" ++ "\n" ++ deviceStrings ds
  where
    props = deviceProperties d
    propStr = unlines $ intersperse ", " $ map (\p -> "\"" ++ deviceProperty p ++ "\": " ++ value p ++ "</P>") props
    str = "<DevI type=\"" ++ deviceType d ++ "\" id=\"" ++ deviceInstanceID d ++ "\""

edgeStrings :: [EdgeInstance] -> String
edgeStrings [] = ""
edgeStrings (e:es) = "\t\t<EdgeI path=\"" ++ (deviceInstanceID $ inNode e) ++ ":in-" ++ (deviceInstanceID $ outNode e) ++ ":out\"/>\n" ++ edgeStrings es
