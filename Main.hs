module Main (main) where

import Encode
import Synthesis
import TechnologyMapping
import System.Process
import System.IO
import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall unsafe "encoding_graphs"
    encoding_graphs :: CString -> CString -> Int -> IO Int

data EncodingType = Single_literal |
                    Sequential |
                    SatBased |
                    Random_encoding |
                    Heuristic |
                    Exhaustive deriving (Enum)

main = do
    putStr "File containing partial orders: "
    hFlush stdout
    fileName <- getLine
    let graphsPath = "test/" ++ fileName

    putStr "File containing custom opcodes: "
    hFlush stdout
    fileName <- getLine
    let encodingSetPath = "test/" ++ fileName

    putStrLn "\t 1) Single-literal encoding"
    putStrLn "\t 2) Sequential encoding"
    putStrLn "\t 3) SAT-based optimal encoding"
    putStrLn "\t 4) Random encoding (supports constraints)"
    putStrLn "\t 5) Heuristic encoding (supports constraints)"
    putStrLn "\t 6) Exhaustive encoding (supports constraints)"
    putStr "Select algorithm to use: "
    hFlush stdout
    encodingNumber <- read <$> getLine
    let encoding = toEnum (encodingNumber - 1)

    graphs <- newCString graphsPath
    encodingSet <- newCString encodingSetPath
    encoding_graphs graphs encodingSet encoding
    putStrLn "Done!"

    -- how to spawn a process, reading the ouput
    --let tool = "./a.out"
    --let fileName = "test1.txt"
    --(_, Just hOut, _, hProc) <- createProcess (proc tool [fileName])
    --    { std_out = CreatePipe }
    --exitCode <- waitForProcess hProc
    --putStrLn exitCode
    --output <- hGetContents hOut
    --putStrLn output
