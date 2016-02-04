module Main (main) where

import Encode
import Synthesis
import TechnologyMapping

main = do
    -- encoding test
    let numberGraphs = 8 --number of graphs to encode
    poFile    <- getPartialOrderFilename
    customOp  <- getCustomEncodingFilename
    algorithm <- getEncodingAlgorithm
    result <- encodeGraphs poFile customOp algorithm -- encode graphs
    if result == 0 then do
        putStrLn "Graphs encoded."
        let opcodeLength = getOpcodesLength -- get opcode length
        opcodes <- getOpcodes numberGraphs opcodeLength -- get opcodes
        print opcodes
    else putStrLn "Encoding failed."


    --if result = 0 then get the matrix

    -- how to spawn a process, reading the ouput
    --let tool = "./a.out"
    --let fileName = "test1.txt"
    --(_, Just hOut, _, hProc) <- createProcess (proc tool [fileName])
    --    { std_out = CreatePipe }
    --exitCode <- waitForProcess hProc
    --putStrLn exitCode
    --output <- hGetContents hOut
    --putStrLn output
