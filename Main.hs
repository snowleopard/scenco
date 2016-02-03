module Main (main) where

import Encode
import Synthesis
import TechnologyMapping

main = do
    -- encoding test
    poFile <- getPartialOrderFilename
    customOp <- getCustomEncodingFilename
    algorithm <- getEncodingAlgorithm
    result <- encodeGraphs poFile customOp algorithm
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
