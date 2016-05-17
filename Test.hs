import Data.List
import System.Directory
import System.FilePath
import System.IO

import Tuura.Abc
import Tuura.Code
import Tuura.Graph
import Tuura.Library
import Tuura.Scenco

testPath :: FilePath
testPath = "test"

techLib :: Library
techLib = loadLibrary (testPath </> "90nm.genlib")

-- automatic generation
nGraphs :: Int
nGraphs = 120

nEvents :: Int
nEvents = 5

main :: IO ()
main = do
    putStrLn scencoVersion
    abcCheck
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
    testAutomaticGeneration nGraphs nEvents

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

testAutomaticGeneration :: Int -> Int -> IO ()
testAutomaticGeneration g e = do
    randGraphs <- generateGraphs g e
    let graphs  = loadGraph    randGraphs
        codes   = getCodesFile ""
    runEncodings graphs codes
    removeFile randGraphs

runTests :: FilePath -> FilePath -> IO ()
runTests cpogPath codesSetPath = do
    let codesPath   = (testPath </> codesSetPath <.> "opcodes")
        codes       = getCodesFile codesPath
        graphsPath  = (testPath </> cpogPath <.> "cpog")
        graphs      = loadGraph graphsPath
    runEncodings graphs codes

runEncodings :: GraphsFile -> CodesFile -> IO ()
runEncodings graphs codes = do
    codeConstraints <- loadGraphsAndCodes graphs codes
    _ <- sequentialSearch graphs techLib CPOG
    _ <- singleLiteralSearch graphs techLib CPOG
    _ <- randomSearch graphs techLib codeConstraints 10 CPOG
    _ <- heuristicSearch graphs techLib codeConstraints 10 CPOG
    unloadGraphs

generateGraphs :: Int -> Int -> IO (FilePath)
generateGraphs ng ne = do
    putStrLn ("========== Random graphs generation ("
             ++ show ng ++ " Partial orders - " ++ show ne ++ " Events)")
    let graphsNumber = take ng $ permutations [1..ne]
    putStrLn (show (length graphsNumber) ++ " graphs generated.")
    (tempPath,tempHandle) <- openTempFile testPath "g.cpog"
    hPutStr tempHandle (convertIntoScenarios 1 (convertIntoStrings graphsNumber))
    hClose tempHandle
    return (tempPath)

convertIntoStrings :: [[Int]] -> [String]
convertIntoStrings []     = []
convertIntoStrings (x:xs) = convertIntoString x : convertIntoStrings xs

convertIntoString :: [Int] -> String
convertIntoString []     = "\n"
convertIntoString (e:es) = "event_" ++ show e ++ " " ++ convertIntoString es

convertIntoScenarios :: Int -> [String] -> String
convertIntoScenarios _ [] = ""
convertIntoScenarios n (x:xs) =   (".scenario " ++ "graph_" ++ show n ++ "\n")
                               ++ x
                               ++ ".end\n\n"
                               ++ convertIntoScenarios (n+1) xs
