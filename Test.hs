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
abcPath = ("abc")

techLibPath :: FilePath
techLibPath = (testPath </> "90nm.genlib")

testArm8 :: IO ()
testArm8 = do
    putStrLn "========== ARM Cortex M0+ (8 Partial orders)"
    runAsserts "arm_8.cpog" "arm_8.opcodes" 8
    putStrLn "=========="

testArm11 :: IO ()
testArm11 = do
    putStrLn "========== ARM Cortex M0+ (11 Partial orders)"
    runAsserts "arm_11.cpog" "arm_11.opcodes" 11
    putStrLn "=========="

testIntel7 :: IO ()
testIntel7 = do
    putStrLn "========== Intel 8051 (7 Partial orders)"
    runAsserts "Intel8051_7.cpog" "Intel8051_7.opcodes" 7
    putStrLn "=========="

testIntel8 :: IO ()
testIntel8 = do
    putStrLn "========== Intel 8051 (8 Partial orders)"
    runAsserts "Intel8051_8.cpog" "Intel8051_8.opcodes" 8
    putStrLn "=========="

testIntel9 :: IO ()
testIntel9 = do
    putStrLn "========== Intel 8051 (9 Partial orders)"
    runAsserts "Intel8051_9.cpog" "Intel8051_9.opcodes" 9
    putStrLn "=========="

testTexasInstrument7 :: IO ()
testTexasInstrument7 = do
    putStrLn "========== Texas Instrument MSP 430 (7 Partial orders)"
    runAsserts "TI_MSP_430_7.cpog" "TI_MSP_430_7.opcodes" 7
    putStrLn "=========="

testTexasInstrument8 :: IO ()
testTexasInstrument8 = do
    putStrLn "========== Texas Instrument MSP 430 (8 Partial orders)"
    runAsserts "TI_MSP_430_8.cpog" "TI_MSP_430_8.opcodes" 8
    putStrLn "=========="

runAsserts :: FilePath -> FilePath -> Int -> IO ()
runAsserts cpogFile codesFile numPartialOrders = do
    assertLoad (testPath </> cpogFile) (testPath </> codesFile)
    customCodes <- parseCustomCode (testPath </> codesFile)
    runAllAlgorithms numPartialOrders customCodes
    assertUnload

runAllAlgorithms :: Int -> [CodeWithUnknowns] -> IO ()
runAllAlgorithms numPartialOrders customCodes = do
    assertSingleLiteral
    assertSynthesis "single.v"
    assertSequential
    assertSynthesis "sequential.v"
    assertRandom numPartialOrders customCodes
    assertSynthesis "random.v"
    assertHeuristic numPartialOrders customCodes
    assertSynthesis "heuristic.v"

testEncoding :: [CodeWithUnknowns] -> [CodeWithoutUnknowns] -> IO ()
testEncoding [] [] = putStrLn "Valid encoding"
testEncoding [] ys = error $ "Extra codes found " ++ show ys
testEncoding xs [] = error $ "Missing codes for " ++ show xs
testEncoding (x:xs) (y:ys) = do
    let result = validate x y
    when (result /= Valid) . error $
        show result ++ ": " ++ show x ++ " => " ++ show y
    testEncoding xs ys

assertSynthesis :: FilePath -> IO ()
assertSynthesis verilogPath = do
    result <- synthesis abcPath Controller
    check result "\tBoolean equations generation: OK\n" "Boolean equations generation: ERROR"
    area <- estimateArea abcPath techLibPath
    putStrLn ("\tArea of the controller: " ++ show area)
--  resultV <- writeVerilog abcPath techLibPath verilogPath
--  check resultV "\tVerilog file generation: OK\n" "Verilog file generation: ERROR"
    unloadController

check :: Int -> String -> String -> IO ()
check result msgOk msgError
    | result == 0 = putStr msgOk
    | otherwise   = error $ msgError ++ " (code " ++ show result ++ ")"

assertLoad :: FilePath -> FilePath -> IO ()
assertLoad cpogFile codesFile = do
    result <- loadGraphsAndCodes cpogFile codesFile
    check result "Graphs and codes loaded\n" "Error loading graphs"

assertUnload :: IO ()
assertUnload = do
    result <- unloadGraphsAndCodes
    check result "Graphs and codes unloaded\n" "Error unloading graphs"

assertSingleLiteral :: IO ()
assertSingleLiteral = do
    result <- encodeGraphs SingleLiteral Nothing
    check result "> Single literal encoding: OK\n" "Single literal encoding: ERROR"

assertSequential :: IO ()
assertSequential = do
    result <- encodeGraphs Sequential Nothing
    check result "> Sequential encoding: OK\n" "Sequential encoding: ERROR"

assertRandom :: Int -> [CodeWithUnknowns] -> IO ()
assertRandom numPartialOrders customCodes = do
    result <- encodeGraphs Random (Just 100)
    check result "> Random encoding..." "Random encoding: ERROR"
    randomBits <- getCodesLength
    runCodeAssert randomBits numPartialOrders customCodes

assertHeuristic :: Int -> [CodeWithUnknowns] -> IO ()
assertHeuristic numPartialOrders customCodes = do
    result <- encodeGraphs Heuristic (Just 100)
    check result "> Heuristic encoding..." "Heuristic encoding: ERROR"
    heuristicBits <- getCodesLength
    runCodeAssert heuristicBits numPartialOrders customCodes

runCodeAssert :: Int -> Int -> [CodeWithUnknowns] -> IO ()
runCodeAssert bitLength numPartialOrders customCodes = do
    generatedCodes <- getCodes numPartialOrders bitLength
    testEncoding customCodes generatedCodes
