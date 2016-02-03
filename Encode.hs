module Encode (encode, overlay, getPartialOrderFilename,
                getCustomEncodingFilename, getEncodingAlgorithm,
                encodeGraphs) where

import Code
import Graph
import System.IO
import Foreign
import Foreign.C.Types
import Foreign.C.String

testFolder = "test/"

foreign import ccall unsafe "encoding_graphs"
    encoding_graphs :: CString -> CString -> Int -> IO Int

foreign import ccall unsafe "get_bit"
    get_bit :: CInt -> CInt -> IO Int

data EncodingType = Single_literal |
                    Sequential |
                    SatBased |
                    Random_encoding |
                    Heuristic |
                    Exhaustive deriving (Enum)

data BitType = ZERO | ONE | UNKNOWN | DONT_USE deriving (Enum)

-- Guarantees:
-- 1) Turn unknowns to knowns:
--     Known True -> Known True
--     Known False -> Known False
--     Unused -> Unused
--     Unknown -> Known True or Known False
-- 2) The number codes should be the same -- give up on this
-- 3) No codes are conflicting

-- Possible reasons to fail:
-- 1) Input codes are conflicting
-- 2) Input codes are not feasible (not enough bits)
-- 3) Internal error: gurantees are not satisfied

-- gets path of file that contains the partial orders
getPartialOrderFilename :: IO String
getPartialOrderFilename = do
    putStr "File containing partial orders: "
    hFlush stdout
    fileName <- getLine
    let graphsPath = testFolder ++ fileName
    return graphsPath

-- gets path of file that contains the custom opcodes
getCustomEncodingFilename :: IO String
getCustomEncodingFilename = do
    putStr "File containing custom opcodes: "
    hFlush stdout
    fileName <- getLine
    let encodingSetPath = testFolder ++ fileName
    return encodingSetPath

-- lets user select which algorithm to use for the encoding
getEncodingAlgorithm :: IO Int
getEncodingAlgorithm = do
    putStrLn "\t 1) Single-literal encoding"
    putStrLn "\t 2) Sequential encoding"
    putStrLn "\t 3) SAT-based optimal encoding"
    putStrLn "\t 4) Random encoding (supports constraints)"
    putStrLn "\t 5) Heuristic encoding (supports constraints)"
    putStrLn "\t 6) Exhaustive encoding (supports constraints)"
    putStr "Select algorithm to use: "
    hFlush stdout
    encodingNumber <- read <$> getLine
    return (encodingNumber - 1)

-- uses c++ function to encode the partial orders
encodeGraphs :: String -> String -> Int -> IO Int
encodeGraphs graphsPath encodingSetPath encoding = do
    graphs <- newCString graphsPath
    encodingSet <- newCString encodingSetPath
    result <- encoding_graphs graphs encodingSet encoding
    putStrLn "Graphs encoded."
    return result

getOpcodes :: IO [[Int]]
getOpcodes = do
    

encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
encode = undefined

overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
overlay = undefined
