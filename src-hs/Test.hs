module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9) where

import Encode
import Code

testFolder = "test/"

testArm8 :: IO ()
testArm8 = do
    putStrLn "========== ARM Cortex M0+ (8 Partial orders)"
    let cpogFile         = "test/arm_8.cpog"
        codesFile      = "test/arm_8.opcodes"
        numPartialOrders = 8

    assertLoad cpogFile codesFile

    customCodes <- parseCustomCode codesFile
    runAllAlgorithms numPartialOrders customCodes

    assertUnload
    putStrLn "=========="

testArm11 :: IO ()
testArm11 = do
    putStrLn "========== ARM Cortex M0+ (11 Partial orders)"
    let cpogFile         = testFolder ++ "arm_11.cpog"
        codesFile      = testFolder ++ "arm_11.opcodes"
        numPartialOrders = 11

    assertLoad cpogFile codesFile

    customCodes <- parseCustomCode codesFile
    runAllAlgorithms numPartialOrders customCodes

    assertUnload
    putStrLn "=========="

testIntel7 :: IO ()
testIntel7 = do
    putStrLn "========== Intel 8051 (7 Partial orders)"
    let cpogFile         = testFolder ++ "Intel8051_7.cpog"
        codesFile      = testFolder ++ "Intel8051_7.opcodes"
        numPartialOrders = 7

    assertLoad cpogFile codesFile

    customCodes <- parseCustomCode codesFile
    runAllAlgorithms numPartialOrders customCodes

    assertUnload
    putStrLn "=========="

testIntel8 :: IO ()
testIntel8 = do
    putStrLn "========== Intel 8051 (8 Partial orders)"
    let cpogFile         = testFolder ++ "Intel8051_8.cpog"
        codesFile      = testFolder ++ "Intel8051_8.opcodes"
        numPartialOrders = 8

    assertLoad cpogFile codesFile

    customCodes <- parseCustomCode codesFile
    runAllAlgorithms numPartialOrders customCodes

    assertUnload
    putStrLn "=========="

testIntel9 :: IO ()
testIntel9 = do
    putStrLn "========== Intel 8051 (9 Partial orders)"
    let cpogFile         = testFolder ++ "Intel8051_9.cpog"
        codesFile      = testFolder ++ "Intel8051_9.opcodes"
        numPartialOrders = 9

    assertLoad cpogFile codesFile

    customCodes <- parseCustomCode codesFile
    runAllAlgorithms numPartialOrders customCodes

    assertUnload
    putStrLn "=========="

runAllAlgorithms :: Int -> [CodeWithUnknowns] -> IO ()
runAllAlgorithms numPartialOrders customCodes = do
    assertSingleLiteral
    assertSequential
    assertRandom numPartialOrders customCodes
    assertHeuristic numPartialOrders customCodes
    --assertExhaustive

assertCodes :: [CodeWithUnknowns] -> [CodeWithoutUnknowns] -> IO ()
assertCodes [] [] = putStrLn "Valid code"
assertCodes (x:xs) (y:ys) = do
    let codeCorrectness = validate x y
    case codeCorrectness of
        Valid           -> putStr ""
        LengthMismatch  -> putStrLn "Length mismatch"
        UnusedBitAdded  -> putStrLn "Unused bit added"
        KnownBitChanged -> putStrLn "Known bit changed"
        otherwise       -> putStrLn "Invalid code"
    assertCodes xs ys
assertCodes a b = putStrLn "Number of codes mismatch"

assertLoad :: FilePath -> FilePath -> IO ()
assertLoad cpogFile codesFile = do
    result <- loadGraphsAndCodes cpogFile codesFile
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and codes loaded"

assertUnload :: IO ()
assertUnload = do
    result <- unloadGraphsAndCodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and codes unloaded"

assertSingleLiteral :: IO ()
assertSingleLiteral = do
    result <- encodeGraphs SingleLiteral Nothing
    if result /= 0
        then error $ "Single literal encoding... ERROR"
        else putStrLn "Single literal encoding... OK"

assertSequential :: IO ()
assertSequential = do
    result <- encodeGraphs Sequential Nothing
    if result /= 0
        then error $ "Sequential encoding... ERROR"
        else putStrLn "Sequential encoding... OK"

assertRandom :: Int -> [CodeWithUnknowns] -> IO ()
assertRandom numPartialOrders customCodes = do
    result <- encodeGraphs Random (Just 10)
    if result /= 0
        then error $ "Random encoding... ERROR"
        else putStr "Random encoding... "

    let bitLength = getCodesLength
    randomCodes <- getCodes numPartialOrders bitLength
    assertCodes customCodes randomCodes

assertHeuristic :: Int -> [CodeWithUnknowns] -> IO ()
assertHeuristic numPartialOrders customCodes = do
    result <- encodeGraphs Heuristic (Just 10)
    if result /= 0
        then error $ "Heuristic encoding... ERROR"
        else putStr "Heuristic encoding... "

    let bitLength = getCodesLength
    heuristicCodes <- getCodes numPartialOrders bitLength
    assertCodes customCodes heuristicCodes

assertExhaustive :: IO ()
assertExhaustive = do
    result <- encodeGraphs Exhaustive (Just 10)
    if result /= 0
        then error $ "Exhaustive encoding... ERROR"
        else putStrLn "Heuristic encoding... OK"
