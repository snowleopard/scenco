module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9) where

import Encode
import Code

testFolder = "test/"

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

runAsserts :: String -> String -> Int -> IO ()
runAsserts cpogFile codesFile numPartialOrders = do
    assertLoad (testFolder ++ cpogFile) (testFolder ++ codesFile)
    customCodes <- parseCustomCode (testFolder ++ codesFile)
    runAllAlgorithms numPartialOrders customCodes
    assertUnload

runAllAlgorithms :: Int -> [CodeWithUnknowns] -> IO ()
runAllAlgorithms numPartialOrders customCodes = do
    assertSingleLiteral
    assertSequential
    assertRandom numPartialOrders customCodes
    assertHeuristic numPartialOrders customCodes
    --assertExhaustive numPartialOrders customCodes

assertCodes :: [CodeWithUnknowns] -> [CodeWithoutUnknowns] -> IO ()
assertCodes [] [] = putStrLn "Valid code"
assertCodes (x:xs) (y:ys) = do
    let codeCorrectness = validate x y
    case codeCorrectness of
        Valid            -> putStr ""
        LengthMismatch   -> putStrLn "Length mismatch"
        UnusedBitRemoved -> putStrLn "Unused bit removed"
        UnusedBitAdded   -> putStrLn "Unused bit added"
        KnownBitChanged  -> putStrLn "Known bit changed"
    assertCodes xs ys

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
    runCodeAssert getCodesLength numPartialOrders customCodes

assertHeuristic :: Int -> [CodeWithUnknowns] -> IO ()
assertHeuristic numPartialOrders customCodes = do
    result <- encodeGraphs Heuristic (Just 10)
    if result /= 0
        then error $ "Heuristic encoding... ERROR"
        else putStr "Heuristic encoding... "
    runCodeAssert getCodesLength numPartialOrders customCodes

assertExhaustive :: Int -> [CodeWithUnknowns] -> IO ()
assertExhaustive numPartialOrders customCodes = do
    result <- encodeGraphs Exhaustive (Just 10)
    if result /= 0
        then error $ "Exhaustive encoding... ERROR"
        else putStrLn "Heuristic encoding... OK"
    runCodeAssert getCodesLength numPartialOrders customCodes

runCodeAssert :: Int -> Int -> [CodeWithUnknowns] -> IO ()
runCodeAssert bitLength numPartialOrders customCodes = do
    randomCodes <- getCodes numPartialOrders bitLength
    assertCodes customCodes randomCodes
