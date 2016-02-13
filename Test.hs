module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9) where

import System.FilePath
import Control.Monad

import Tuura.Encode
import Tuura.Code

testPath :: FilePath
testPath = "test"

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

runAsserts :: FilePath -> FilePath -> Int -> IO ()
runAsserts cpogFile codesFile numPartialOrders = do
    assertLoad (testPath </> cpogFile) (testPath </> codesFile)
    customCodes <- parseCustomCode (testPath </> codesFile)
    runAllAlgorithms numPartialOrders customCodes
    assertUnload

runAllAlgorithms :: Int -> [CodeWithUnknowns] -> IO ()
runAllAlgorithms numPartialOrders customCodes = do
    assertSingleLiteral
    assertSequential
    assertRandom numPartialOrders customCodes
    assertHeuristic numPartialOrders customCodes
    --assertExhaustive numPartialOrders customCodes

testEncoding :: [CodeWithUnknowns] -> [CodeWithoutUnknowns] -> IO ()
testEncoding [] [] = putStrLn "Valid encoding"
testEncoding [] ys = error $ "Extra codes found " ++ show ys
testEncoding xs [] = error $ "Missing codes for " ++ show xs
testEncoding (x:xs) (y:ys) = do
    let result = validate x y
    when (result /= Valid) . error $
        show result ++ ": " ++ show x ++ " => " ++ show y
    testEncoding xs ys

check :: Int -> String -> String -> IO ()
check result msgOk msgError
    | result == 0 = putStrLn msgOk
    | otherwise   = error $ msgError ++ " (code " ++ show result ++ ")"

assertLoad :: FilePath -> FilePath -> IO ()
assertLoad cpogFile codesFile = do
    result <- loadGraphsAndCodes cpogFile codesFile
    check result "Graphs and codes loaded" "Error loading graphs"

assertUnload :: IO ()
assertUnload = do
    result <- unloadGraphsAndCodes
    check result "Graphs and codes unloaded" "Error unloading graphs"

assertSingleLiteral :: IO ()
assertSingleLiteral = do
    result <- encodeGraphs SingleLiteral Nothing
    check result "Single literal encoding: OK" "Single literal encoding: ERROR"

assertSequential :: IO ()
assertSequential = do
    result <- encodeGraphs Sequential Nothing
    check result "Sequential encoding: OK" "Sequential encoding: ERROR"

assertRandom :: Int -> [CodeWithUnknowns] -> IO ()
assertRandom numPartialOrders customCodes = do
    result <- encodeGraphs Random (Just 10)
    check result "Random encoding..." "Random encoding: ERROR"
    runCodeAssert getCodesLength numPartialOrders customCodes

assertHeuristic :: Int -> [CodeWithUnknowns] -> IO ()
assertHeuristic numPartialOrders customCodes = do
    result <- encodeGraphs Heuristic (Just 10)
    check result "Heuristic encoding..." "Heuristic encoding: ERROR"
    runCodeAssert getCodesLength numPartialOrders customCodes

-- TODO: Do we need this? If not, remove.
-- assertExhaustive :: Int -> [CodeWithUnknowns] -> IO ()
-- assertExhaustive numPartialOrders customCodes = do
--     result <- encodeGraphs Exhaustive (Just 10)
--     if result /= 0
--         then error $ "Exhaustive encoding... ERROR"
--         else putStrLn "Heuristic encoding... OK"
--     runCodeAssert getCodesLength numPartialOrders customCodes

runCodeAssert :: Int -> Int -> [CodeWithUnknowns] -> IO ()
runCodeAssert bitLength numPartialOrders customCodes = do
    randomCodes <- getCodes numPartialOrders bitLength
    testEncoding customCodes randomCodes
