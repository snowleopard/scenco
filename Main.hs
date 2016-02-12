module Main (main) where

import Test

scencoVersion :: String
scencoVersion = "Scenco v0.1"

main :: IO ()
main = do
    putStrLn scencoVersion
    putStrLn ""
    testArm8
    testArm11
    testIntel7
    testIntel8
    testIntel9
