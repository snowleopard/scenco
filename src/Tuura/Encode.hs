module Tuura.Encode (loadGraphsAndCodes, encodeGraphs,
               getCodesLength, getCodes,unloadGraphsAndCodes,
               EncodingType(..)) where

import Foreign.C.String
import Tuura.Code

foreign import ccall unsafe "load_graphs_codes"
    insertGraphsAndCodes :: CString -> CString -> IO Int

foreign import ccall unsafe "unload_graphs_codes"
    unloadGraphsAndCodes :: IO Int

foreign import ccall unsafe "get_bit"
    getBit :: Int -> Int -> IO Int

foreign import ccall unsafe "get_codes_length"
    getCodesLength :: Int

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

-- uses c++ function to encode the partial orders
loadGraphsAndCodes :: FilePath -> FilePath -> IO Int
loadGraphsAndCodes graphsPath encodingSetPath = do
    graphs      <- newCString graphsPath
    encodingSet <- newCString encodingSetPath
    insertGraphsAndCodes graphs encodingSet

getCodeBit :: Int -> Int -> IO (Bit Bool)
getCodeBit partialOrder bitPosition = do
    bitC <- getBit partialOrder bitPosition
    return $ convert bitC

convert :: Int -> Bit Bool
convert x
    | x == 0    = used False
    | x == 1    = used True
    | x == 2    = unused
    | otherwise = error $ "Cannot convert " ++ show x ++ " to Bit Bool"

encodeGraphs :: EncodingType -> Maybe Int -> IO Int
encodeGraphs SingleLiteral      _       = singleLiteralEncoding
encodeGraphs Sequential         _       = sequentialEncoding
encodeGraphs Random         (Just a)    = randomEncoding        a
encodeGraphs Random             _       = randomEncoding        10
encodeGraphs Heuristic      (Just a)    = heuristicEncoding     a
encodeGraphs Heuristic          _       = heuristicEncoding     10
encodeGraphs Exhaustive     (Just a)    = exhaustiveEncoding    a
encodeGraphs Exhaustive         _       = exhaustiveEncoding    10

getCode :: Int -> Int -> IO CodeWithoutUnknowns
getCode bitLength poID = traverse (getCodeBit poID) [0..bitLength-1]

getCodes :: Int -> Int -> IO [CodeWithoutUnknowns]
getCodes nPO bitLength = traverse (getCode bitLength) [0..nPO-1]

--encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
--encode = undefined

--overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
--overlay = undefined
