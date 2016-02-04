module Encode (getPartialOrderFilename,
               getCustomEncodingFilename, getEncodingAlgorithm,
               encodeGraphs, getOpcodesLength, getOpcodes) where

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
    get_bit :: Int -> Int -> IO Int

foreign import ccall unsafe "get_opcodes_length"
    getOpcodesLength :: Int

data EncodingType = Single_literal |
                    Sequential |
                    SatBased |
                    Random_encoding |
                    Heuristic |
                    Exhaustive deriving (Enum)

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
    return result

getOpcodeBit :: Int -> Int -> IO (Bit Bool)
getOpcodeBit partialOrder bitPosition = do
    bitC <- get_bit partialOrder bitPosition
    return $ convert bitC

convert :: Int -> Bit Bool
convert x | x == 0    = used False
          | x == 1    = used True
          | x == 2    = unused
          | otherwise = error $ "Cannot convert " ++ show x ++ " to Bit Bool"

getOpcode :: Int -> Int -> IO CodeWithoutUnknowns
getOpcode bitLength poID = traverse (getOpcodeBit poID) [0..bitLength-1]

getOpcodes :: Int -> Int -> IO [CodeWithoutUnknowns]
getOpcodes nPO bitLength = traverse (getOpcode bitLength) [0..nPO-1]

--encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
--encode = undefined

--overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
--overlay = undefined
