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
    encoding_graphs :: CString -> Int -> IO Int

main = do
    putStrLn "SCENCO v.0.1"

    -- how to spawn a process, reading the ouput
    --let tool = "./a.out"
    --let fileName = "test1.txt"
    --(_, Just hOut, _, hProc) <- createProcess (proc tool [fileName])
    --    { std_out = CreatePipe }
    --exitCode <- waitForProcess hProc
    --putStrLn exitCode
    --output <- hGetContents hOut
    --putStrLn output

    putStrLn "Printing from c.."
    fileName <- newCString "arm.cpog"
    encoding_graphs fileName 1
    putStrLn "Done!"
