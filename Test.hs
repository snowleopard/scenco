module Test (testArm8, testArm11, testIntel7, testIntel8, testIntel9) where

import Encode
import System.IO

testArm8 :: IO Bool
testArm8 = do
    putStrLn "======= ARM Cortex M0+ (8 Partial orders)"
    result <- loadGraphsAndOpcodes "test/arm_8.cpog" "test/arm_8.opcodes"
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and opcodes loaded"

    runAllAlgorithms
    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes 8 opcodeLength -- get opcodes

    result <- unloadGraphsAndOpcodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and opcodes unloaded"
    putStrLn "======="
    return True

testArm11 :: IO Bool
testArm11 = do
    putStrLn "======= ARM Cortex M0+ (11 Partial orders)"
    result <- loadGraphsAndOpcodes "test/arm_11.cpog" "test/arm_11.opcodes"
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and opcodes loaded"

    runAllAlgorithms
    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes 11 opcodeLength -- get opcodes

    result <- unloadGraphsAndOpcodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and opcodes unloaded"
    putStrLn "======="
    return True

testIntel7 :: IO Bool
testIntel7 = do
    putStrLn "======= Intel 8051 (7 Partial orders)"
    result <- loadGraphsAndOpcodes "test/Intel8051_7.cpog" "test/Intel8051_7.opcodes"
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and opcodes loaded"

    runAllAlgorithms
    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes 7 opcodeLength -- get opcodes

    result <- unloadGraphsAndOpcodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and opcodes unloaded"
    putStrLn "======="
    return True

testIntel8 :: IO Bool
testIntel8 = do
    putStrLn "======= Intel 8051 (8 Partial orders)"
    result <- loadGraphsAndOpcodes "test/Intel8051_8.cpog" "test/Intel8051_8.opcodes"
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and opcodes loaded"

    runAllAlgorithms
    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes 8 opcodeLength -- get opcodes

    result <- unloadGraphsAndOpcodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and opcodes unloaded"
    putStrLn "======="
    return True

testIntel9 :: IO Bool
testIntel9 = do
    putStrLn "======= Intel 8051 (9 Partial orders)"
    result <- loadGraphsAndOpcodes "test/Intel8051_9.cpog" "test/Intel8051_9.opcodes"
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs and opcodes loaded"

    runAllAlgorithms
    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes 9 opcodeLength -- get opcodes

    result <- unloadGraphsAndOpcodes
    if result /= 0
        then error $ "Error unloading graphs"
        else putStrLn "Graphs and opcodes unloaded"
    putStrLn "======="
    return True

runAllAlgorithms :: IO ()
runAllAlgorithms = do
    result <- encodeGraphs SingleLiteral
    if result /= 0
        then error $ "Single literal encoding... ERROR"
        else putStrLn "Single literal encoding... OK"

    result <- encodeGraphs Sequential
    if result /= 0
        then error $ "Sequential encoding... ERROR"
        else putStrLn "Sequential encoding... OK"

    result <- encodeGraphs Random
    if result /= 0
        then error $ "Random encoding... ERROR"
        else putStrLn "Random encoding... OK"

    result <- encodeGraphs Heuristic
    if result /= 0
        then error $ "Heuristic encoding... ERROR"
        else putStrLn "Heuristic encoding... OK"
