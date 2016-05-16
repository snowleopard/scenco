import Tuura.Scenco
import Tuura.Code
import Tuura.Graph
import Tuura.Library

import System.FilePath

testPath :: FilePath
testPath = "test"

techLibPath :: FilePath
techLibPath = (testPath </> "90nm.genlib")

main :: IO ()
main = do
    putStrLn scencoVersion
    executeTests

executeTests :: IO ()
executeTests = do
    testArm8
    testArm11
    testIntel7
    testIntel8
    testIntel9
    testTexasInstrument7
    testTexasInstrument8

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
runTests cpogPath codesSetPath = do
    let codesPath   = (testPath </> codesSetPath <.> "opcodes")
        codes       = getCodesFile codesPath
        graphsPath  = (testPath </> cpogPath <.> "cpog")
        graphs      = loadGraph graphsPath
        techLib     = loadLibrary techLibPath
    codeConstraints <- parseCustomCode codes
    _ <- loadGraphsAndCodes graphs codes
    _ <- sequentialSearch graphs techLib CPOG
    _ <- singleLiteralSearch graphs techLib CPOG
    _ <- randomSearch graphs techLib codeConstraints 10 CPOG
    _ <- heuristicSearch graphs techLib codeConstraints 10 CPOG
    unloadGraphs
