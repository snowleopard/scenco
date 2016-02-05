module Main (main) where

import Encode
import Synthesis
import TechnologyMapping

main = do
    -- encoding test
    let numberGraphs = 11 --number of graphs to encode
    poFile    <- getPartialOrderFilename
    customOp  <- getCustomEncodingFilename
    result <- loadGraphsAndOpcodes poFile customOp
    if result /= 0
        then error $ "Error loading graphs"
        else putStrLn "Graphs loaded"

    algorithm <- getEncodingAlgorithm
    result <- encodeGraphs algorithm
    if result /= 0
        then error $ "Error encoding the graphs"
        else putStrLn "Graphs encoded"

    let opcodeLength = getOpcodesLength -- get opcode length
    opcodes <- getOpcodes numberGraphs opcodeLength -- get opcodes
    print opcodes
