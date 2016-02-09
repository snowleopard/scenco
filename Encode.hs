module Encode (getPartialOrderFilename, getCustomEncodingFilename,
               getEncodingAlgorithm, loadGraphsAndOpcodes, encodeGraphs,
               getOpcodesLength, getOpcodes,unloadGraphsAndOpcodes,
               EncodingType(..)) where

import Code
import Graph
import System.IO
import Foreign
import Foreign.C.Types
import Foreign.C.String

testFolder = "test/"

foreign import ccall unsafe "load_graphs_opcodes"
    insertGraphsAndOpcodes :: CString -> CString -> IO Int

foreign import ccall unsafe "unload_graphs_opcodes"
    unloadGraphsAndOpcodes :: IO Int

foreign import ccall unsafe "get_bit"
    getBit :: Int -> Int -> IO Int

foreign import ccall unsafe "get_opcodes_length"
    getOpcodesLength :: Int

foreign import ccall unsafe "single_literal_encoding"
    singleLiteralEncoding :: IO Int

foreign import ccall unsafe "sequential_encoding"
    sequentialEncoding :: IO Int

foreign import ccall unsafe "random_encoding"
    randomEncoding :: Int -> IO Int

foreign import ccall unsafe "heuristic_encoding"
    heuristicEncoding :: Int -> IO Int

foreign import ccall unsafe "exhaustive_encoding"
    exhaustiveEncoding :: Int -> IO Int

data EncodingType = SingleLiteral
                  | Sequential
                  | Random
                  | Heuristic
                  | Exhaustive

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
getEncodingAlgorithm :: IO EncodingType
getEncodingAlgorithm = do
    putStrLn " "
    putStrLn "Algorithms available for encoding the graphs:"
    putStrLn "\t 1) Single-literal encoding"
    putStrLn "\t 2) Sequential encoding"
    putStrLn "\t 3) Random encoding (supports constraints)"
    putStrLn "\t 4) Heuristic encoding (supports constraints)"
    putStrLn "\t 5) Exhaustive encoding (supports constraints)"
    putStr "Select the number of the algorithm you want to use: "
    hFlush stdout
    newstdin <- openFile "/dev/tty" ReadMode
    --encodingId <- readNumber
    --encodingId <- read <$> getLine
    encodingId <- read <$> (hGetLine newstdin)
    hClose newstdin
    return $ convertAlgorithm encodingId

-- uses c++ function to encode the partial orders
loadGraphsAndOpcodes :: String -> String -> IO Int
loadGraphsAndOpcodes graphsPath encodingSetPath = do
    graphs <- newCString graphsPath
    encodingSet <- newCString encodingSetPath
    result <- insertGraphsAndOpcodes graphs encodingSet
    return result

getOpcodeBit :: Int -> Int -> IO (Bit Bool)
getOpcodeBit partialOrder bitPosition = do
    bitC <- getBit partialOrder bitPosition
    return $ convert bitC

convert :: Int -> Bit Bool
convert x
    | x == 0    = used False
    | x == 1    = used True
    | x == 2    = unused
    | otherwise = error $ "Cannot convert " ++ show x ++ " to Bit Bool"

convertAlgorithm :: Int -> EncodingType
convertAlgorithm x
    | x == 1     = SingleLiteral
    | x == 2     = Sequential
    | x == 3     = Random
    | x == 4     = Heuristic
    | x == 5     = Exhaustive
    | otherwise  = error $ "Wrong algorithm selected " ++ show x

encodeGraphs :: EncodingType -> Maybe Int -> IO Int
encodeGraphs SingleLiteral _ = singleLiteralEncoding
encodeGraphs Sequential _ = sequentialEncoding
encodeGraphs Random (Just a) = randomEncoding a
encodeGraphs Heuristic (Just a) = heuristicEncoding a
encodeGraphs Exhaustive (Just a) = exhaustiveEncoding a
encodeGraphs x _ = error $ "Wrong algorithm selected"

getOpcode :: Int -> Int -> IO CodeWithoutUnknowns
getOpcode bitLength poID = traverse (getOpcodeBit poID) [0..bitLength-1]

getOpcodes :: Int -> Int -> IO [CodeWithoutUnknowns]
getOpcodes nPO bitLength = traverse (getOpcode bitLength) [0..nPO-1]

--encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
--encode = undefined

--overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
--overlay = undefined
