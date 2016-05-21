import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO

import Tuura.Abc
import Tuura.Code
import Tuura.Encode
import Tuura.Library
import Tuura.Scenco

testPath :: FilePath
testPath = "test"

techLib :: Library
techLib = loadLibrary (testPath </> "90nm.genlib")

-- automatic generation
nGraphs :: Int
nGraphs = 60

nEvents :: Int
nEvents = 5

nCodes :: [Int]
nCodes = [0..5]

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
    testFreeCodes nCodes

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
    let graphs  = GraphFile randGraphs
        codes   = CodeFile ""
    runEncodings graphs codes
    removeFile randGraphs

testFreeCodes :: [Int] -> IO ()
testFreeCodes tests = do
    putStrLn "========== Free constraints codes test"
    putStrLn ("Test code generation from " ++ show (minimum tests)
             ++ " to " ++ show (maximum tests) ++ " graphs")
    when (checkFreeCodes tests == False) . error $
        "Not constrained codes generation failed"
    putStrLn "Free constraints code generation: OK"
    return ()

runTests :: FilePath -> FilePath -> IO ()
runTests cpogPath codesSetPath = do
    let codesPath   = (testPath </> codesSetPath <.> "opcodes")
        codes       = CodeFile codesPath
        graphsPath  = (testPath </> cpogPath <.> "cpog")
        graphs      = GraphFile graphsPath
    runEncodings graphs codes

runEncodings :: GraphFile -> CodeFile -> IO ()
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

checkFreeCodes :: [Int] -> Bool
checkFreeCodes = all checkFreeCode

checkFreeCode :: Int -> Bool
checkFreeCode n = checkCode n (constraintFreeCodes n)

checkCode :: Int -> [CodeWithUnknowns] -> Bool
checkCode 0 = (== [])
checkCode 1 = (== [[]])
checkCode 2 = (== replicate 2 (replicate 1 unknown))
checkCode 3 = (== replicate 3 (replicate 2 unknown))
checkCode 4 = (== replicate 4 (replicate 2 unknown))
checkCode 5 = (== replicate 5 (replicate 3 unknown))
checkCode n = error $ "Test result is not defined for " ++ show n
