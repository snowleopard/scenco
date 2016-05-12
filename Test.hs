module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9,
             testTexasInstrument7, testTexasInstrument8, testLog9,
             testLog167, testLog400) where

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

testLog9 :: IO ()
testLog9 = do
    putStrLn "========== DigitalCopier (9 Partial orders)"
    runLogTests "DigitalCopier" "DigitalCopier"

testLog167 :: IO ()
testLog167 = do
    putStrLn "========== colibrilog (167 Partial orders)"
    runLogTests "colibrilog" "colibrilog"

testLog400 :: IO ()
testLog400 = do
    putStrLn "========== documentflow (400 Partial orders)"
    runLogTests "documentflow" "documentflow"

runTests :: FilePath -> FilePath -> IO ()
runTests cpog codes = do
    let codesPath = (testPath </> codes <.> "opcodes")
        codesFile = loadCodes codesPath
        graphsPath = (testPath </> cpog <.> "cpog")
    codeConstraints <- parseCustomCode codesFile
    loadTest graphsPath codesPath
    testSingleLiteral graphsPath
    testSequential graphsPath --("sequential_literal" <.> "v")
    testRandom graphsPath codeConstraints --("random_literal" <.> "v")
    testHeuristic graphsPath codeConstraints --("heuristic_literal" <.> "v")
    unloadTest

runLogTests :: FilePath -> FilePath -> IO ()
runLogTests cpog codes = do
    let codesPath = (testPath </> codes <.> "opcodes")
        codesFile = loadCodes codesPath
        graphsPath = (testPath </> cpog <.> "cpog")
    codeConstraints <- parseCustomCode codesFile
    loadTest graphsPath codesPath
    testSequential graphsPath --("sequential_literal" <.> "v")
    testHeuristic graphsPath codeConstraints --("heuristic_literal" <.> "v")
    unloadTest

loadTest :: FilePath -> FilePath -> IO ()
loadTest cpogFile codesFile = do
    let graphs = loadGraph cpogFile
        codes = loadCodes codesFile
    result <- loadGraphsAndCodes graphs codes
    let err = readError result
    check err "Graphs and codes loaded" "Error loading graphs"

testSingleLiteral :: FilePath -> IO ()
testSingleLiteral graphsPath = do
    result <- encodeGraphs SingleLiteral Nothing
    let err = readError result
    check err "Single literal encoding: OK" "Single literal encoding: ERROR"
    codeSingleLiteral <- getCodes
    assertSynthesisAndMapping graphsPath codeSingleLiteral

testSequential :: FilePath -> IO ()
testSequential graphsPath = do
    result <- encodeGraphs Sequential Nothing
    let err = readError result
    check err "Sequential encoding: OK" "Sequential encoding: ERROR"
    codesSequential <- getCodes
    assertSynthesisAndMapping graphsPath codesSequential

testRandom :: FilePath -> [CodeWithUnknowns] -> IO ()
testRandom graphsPath codeConstraints = do
    codesFinal <- encode Random (Just 10)
    putStr "Random encoding: "
    shouldMeet codesFinal codeConstraints
    assertSynthesisAndMapping graphsPath codesFinal

testHeuristic :: FilePath -> [CodeWithUnknowns] -> IO ()
testHeuristic graphsPath codeConstraints = do
    codesFinal <- encode Heuristic (Just 10)
    putStr "Heuristic encoding: "
    shouldMeet codesFinal codeConstraints
    assertSynthesisAndMapping graphsPath codesFinal

unloadTest :: IO ()
unloadTest = do
    result <- unloadGraphsAndCodes
    let err = readError result
    check err "Graphs and codes unloaded" "Error unloading graphs"

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

shouldMeet :: [CodeWithoutUnknowns] -> [CodeWithUnknowns] -> IO ()
shouldMeet [] [] = putStrLn "Valid encoding"
shouldMeet xs [] = error $ "Extra codes found " ++ show xs
shouldMeet [] ys = error $ "Missing codes for " ++ show ys
shouldMeet (x:xs) (y:ys) = do
    let result = validate y x
    when (result /= Valid) . error $
        show result ++ ": " ++ show y ++ " => " ++ show x
    shouldMeet xs ys
