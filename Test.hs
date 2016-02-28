module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9,
             testTexasInstrument7, testTexasInstrument8) where

import System.FilePath
import Control.Monad

import Tuura.Encode
import Tuura.Code
import Tuura.Synthesis
import Tuura.TechnologyMapping

testPath :: FilePath
testPath = "test"

abcPath :: FilePath
abcPath = "abc"

techLibPath :: FilePath
techLibPath = (testPath </> "90nm.genlib")

testArm8 :: IO ()
testArm8 = do
    putStrLn "========== ARM Cortex M0+ (8 Partial orders)"
    runTests "arm_8" "arm_8.opcodes" 8

testArm11 :: IO ()
testArm11 = do
    putStrLn "========== ARM Cortex M0+ (11 Partial orders)"
    runTests "arm_11" "arm_11" 11

testIntel7 :: IO ()
testIntel7 = do
    putStrLn "========== Intel 8051 (7 Partial orders)"
    runTests "Intel8051_7" "Intel8051_7" 7

testIntel8 :: IO ()
testIntel8 = do
    putStrLn "========== Intel 8051 (8 Partial orders)"
    runTests "Intel8051_8" "Intel8051_8" 8

testIntel9 :: IO ()
testIntel9 = do
    putStrLn "========== Intel 8051 (9 Partial orders)"
    runTests "Intel8051_9" "Intel8051_9" 9

runTests :: FilePath -> FilePath -> Int -> IO ()
runTests cpog codes numPartialOrders = do
    loadTest (testPath </> cpog <.> "cpog") (testPath </> codes <.> "opcodes")
    codeConstraints <- parseCustomCode (testPath </> codes <.> "opcodes")
    testSingleLiteral
    testSequential
    testRandom numPartialOrders codeConstraints
    testHeuristic numPartialOrders codeConstraints
    unloadTest

assertSynthesis :: FilePath -> IO ()
assertSynthesis verilogPath = do
    synthesis Controller
    area <- estimateArea techLibPath
    putStrLn ("\tArea of the controller: " ++ show area)
--  resultV <- writeVerilog abcPath techLibPath verilogPath
--  check resultV "\tVerilog file generation: OK\n" "Verilog file generation: ERROR"
    unloadController

check :: Int -> String -> String -> IO ()
check result msgOk msgError
    | result == 0 = putStrLn msgOk
    | otherwise   = error $ msgError ++ " (code " ++ show result ++ ")"

loadTest :: FilePath -> FilePath -> IO ()
loadTest cpogFile codesFile = do
    result <- loadGraphsAndCodes cpogFile codesFile
    check result "Graphs and codes loaded\n" "Error loading graphs"

unloadTest :: IO ()
unloadTest = do
    result <- unloadGraphsAndCodes
    check result "Graphs and codes unloaded\n" "Error unloading graphs"

testSingleLiteral :: IO ()
testSingleLiteral = do
    result <- encodeGraphs SingleLiteral Nothing
    check result "> Single literal encoding: OK\n" "Single literal encoding: ERROR"

testSequential :: IO ()
testSequential = do
    result <- encodeGraphs Sequential Nothing
    check result "> Sequential encoding: OK\n" "Sequential encoding: ERROR"

testRandom :: Int -> [CodeWithUnknowns] -> IO ()
testRandom numPartialOrders codeConstraints = do
    result <- encodeGraphs Random (Just 10)
    check result "Random encoding..." "Random encoding: ERROR"
    verifyEncoding getCodesLength numPartialOrders codeConstraints

testHeuristic :: Int -> [CodeWithUnknowns] -> IO ()
testHeuristic numPartialOrders codeConstraints = do
    result <- encodeGraphs Heuristic (Just 10)
    check result "Heuristic encoding..." "Heuristic encoding: ERROR"
    verifyEncoding getCodesLength numPartialOrders codeConstraints

-- testExhaustive :: Int -> [CodeWithUnknowns] -> IO ()
-- testExhaustive numPartialOrders codeConstraints = do
--     result <- encodeGraphs Exhaustive (Just 10)
--     if result /= 0
--         then error $ "Exhaustive encoding... ERROR"
--         else putStrLn "Heuristic encoding... OK"
--     verifyEncoding getCodesLength numPartialOrders codeConstraints

verifyEncoding :: Int -> Int -> [CodeWithUnknowns] -> IO ()
verifyEncoding bitLength numPartialOrders codeConstraints = do
    codes <- getCodes numPartialOrders bitLength
    codes `shouldMeet` codeConstraints

shouldMeet :: [CodeWithoutUnknowns] -> [CodeWithUnknowns] -> IO ()
shouldMeet [] [] = putStrLn "Valid encoding"
shouldMeet xs [] = error $ "Extra codes found " ++ show xs
shouldMeet [] ys = error $ "Missing codes for " ++ show ys
shouldMeet (x:xs) (y:ys) = do
    let result = validate y x
    when (result /= Valid) . error $
        show result ++ ": " ++ show y ++ " => " ++ show x
    shouldMeet xs ys
