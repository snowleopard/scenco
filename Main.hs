module Main (main) where

import Test

scencoVersion :: String
scencoVersion = "Scenco v0.1"

main :: IO ()
main = do
    putStrLn scencoVersion
    testArm8
    testArm11
    testIntel7
    testIntel8
    testIntel9
    testTexasInstrument7
    testTexasInstrument8
    putStrLn " "
    putStrLn "========== Event Log Tests =========="
    putStrLn " "
    testLog9
    testLog167
    testLog400
