module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9,
             testTexasInstrument7, testTexasInstrument8) where

import System.FilePath
import Control.Monad

import Tuura.Encode
import Tuura.Code
import Tuura.Synthesis
import Tuura.TechnologyMapping
import Tuura.Graph
import Tuura.Library

testPath :: FilePath
testPath = "test"

techLibPath :: FilePath
techLibPath = (testPath </> "90nm.genlib")

testArm8 :: IO ()
testArm8 = do
    putStrLn "========== ARM Cortex M0+ (8 Partial orders)"
    runTests "arm_8" "arm_8"

testArm11 :: IO ()
testArm11 = do
    putStrLn "========== ARM Cortex M0+ (11 Partial orders)"
    runTests "arm_11" "arm_11"

testIntel7 :: IO ()
testIntel7 = do
    putStrLn "========== Intel 8051 (7 Partial orders)"
    runTests "Intel8051_7" "Intel8051_7"

testIntel8 :: IO ()
testIntel8 = do
    putStrLn "========== Intel 8051 (8 Partial orders)"
    runTests "Intel8051_8" "Intel8051_8"

testIntel9 :: IO ()
testIntel9 = do
    putStrLn "========== Intel 8051 (9 Partial orders)"
    runTests "Intel8051_9" "Intel8051_9"

testTexasInstrument7 :: IO ()
testTexasInstrument7 = do
    putStrLn "========== Texas Instrument MSP 430 (7 Partial orders)"
    runTests "TI_MSP_430_7" "TI_MSP_430_7"

testTexasInstrument8 :: IO ()
testTexasInstrument8 = do
    putStrLn "========== Texas Instrument MSP 430 (8 Partial orders)"
    runTests "TI_MSP_430_8" "TI_MSP_430_8"

runTests :: FilePath -> FilePath -> IO ()
runTests cpog codes = do
    let codesPath = (testPath </> codes <.> "opcodes")
        codesFile = loadCodes codesPath
    loadTest (testPath </> cpog <.> "cpog") (testPath </> codes <.> "opcodes")
    codeConstraints <- parseCustomCode codesFile
    codeSingleLiteral <- testSingleLiteral
    assertSynthesisAndMapping (testPath </> cpog <.> "cpog") codeSingleLiteral --("single_literal" <.> "v")
    codesSequential <- testSequential
    assertSynthesisAndMapping (testPath </> cpog <.> "cpog") codesSequential --("sequential_literal" <.> "v")
    codesRandom <- testRandom codeConstraints
    assertSynthesisAndMapping (testPath </> cpog <.> "cpog") codesRandom --("random_literal" <.> "v")
    codesHeuristic <- testHeuristic codeConstraints
    assertSynthesisAndMapping (testPath </> cpog <.> "cpog") codesHeuristic --("heuristic_literal" <.> "v")
    unloadTest

assertSynthesisAndMapping :: FilePath -> [CodeWithoutUnknowns] -> IO ()
assertSynthesisAndMapping graphs codes = do
    let graphsFile = loadGraph graphs
        libFile    = loadLibrary techLibPath
    formulae <- synthesiseControllerIO graphsFile codes
    area <- estimateArea libFile formulae
    let size = parseArea area
    putStrLn ("\tArea of the controller: " ++ show size)
--  resultV <- writeVerilog libFile formulae verilogPath
--  let err = readError resultV
--  check err "\tVerilog file generation: OK" "Verilog file generation: ERROR"

check :: Int -> String -> String -> IO ()
check result msgOk msgError
    | result == 0 = putStrLn msgOk
    | otherwise   = error $ msgError ++ " (code " ++ show result ++ ")"

loadTest :: FilePath -> FilePath -> IO ()
loadTest cpogFile codesFile = do
    let graphs = loadGraph cpogFile
        codes = loadCodes codesFile
    result <- loadGraphsAndCodes graphs codes
    let err = readError result
    check err "Graphs and codes loaded" "Error loading graphs"

unloadTest :: IO ()
unloadTest = do
    result <- unloadGraphsAndCodes
    let err = readError result
    check err "Graphs and codes unloaded" "Error unloading graphs"

testSingleLiteral :: IO [CodeWithoutUnknowns]
testSingleLiteral = do
    result <- encodeGraphs SingleLiteral Nothing
    let err = readError result
    check err "Single literal encoding: OK" "Single literal encoding: ERROR"
    getCodes

testSequential :: IO [CodeWithoutUnknowns]
testSequential = do
    result <- encodeGraphs Sequential Nothing
    let err = readError result
    check err "Sequential encoding: OK" "Sequential encoding: ERROR"
    getCodes

testRandom :: [CodeWithUnknowns] -> IO [CodeWithoutUnknowns]
testRandom codeConstraints = do
    codesFinal <- encode Random (Just 10)
    putStr "Random encoding: "
    shouldMeet codesFinal codeConstraints
    return codesFinal

testHeuristic :: [CodeWithUnknowns] -> IO [CodeWithoutUnknowns]
testHeuristic codeConstraints = do
    codesFinal <- encode Heuristic (Just 10)
    putStr "Heuristic encoding: "
    shouldMeet codesFinal codeConstraints
    return codesFinal

-- testExhaustive :: Int -> [CodeWithUnknowns] -> IO ()
-- testExhaustive numPartialOrders codeConstraints = do
--     result <- encodeGraphs Exhaustive (Just 10)
--     if result /= 0
--         then error $ "Exhaustive encoding... ERROR"
--         else putStrLn "Heuristic encoding... OK"
--     verifyEncoding getCodesLength numPartialOrders codeConstraints

shouldMeet :: [CodeWithoutUnknowns] -> [CodeWithUnknowns] -> IO ()
shouldMeet [] [] = putStrLn "Valid encoding"
shouldMeet xs [] = error $ "Extra codes found " ++ show xs
shouldMeet [] ys = error $ "Missing codes for " ++ show ys
shouldMeet (x:xs) (y:ys) = do
    let result = validate y x
    when (result /= Valid) . error $
        show result ++ ": " ++ show y ++ " => " ++ show x
    shouldMeet xs ys
