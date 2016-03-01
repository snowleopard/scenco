module Tuura.Encode (loadGraphsAndCodes, encode, encodeGraphs, getCodes,
                     unloadGraphsAndCodes, EncodingType(..), ErrorCode,
                     readError) where

import Tuura.Code
import Tuura.Graph
import Foreign.C.String
import Control.Monad

newtype ErrorCode = ErrorCode Int

readError :: ErrorCode -> Int
readError (ErrorCode err) = err

data EncodingType = SingleLiteral
                  | Sequential
                  | Random
                  | Heuristic
                  | Exhaustive

loadGraphsAndCodes :: GraphsFile -> CodesFile -> IO ErrorCode
loadGraphsAndCodes graphsPath codesPath = do
    let graphsF = graphFilepath graphsPath
        codesF = codesFilepath codesPath
    graphs      <- newCString graphsF
    codesConstraints <- newCString codesF
    insertGraphsAndCodes graphs codesConstraints

encode :: EncodingType -> Maybe Int -> IO [CodeWithoutUnknowns]
encode algorithm nEncoding = do
    result <- encodeGraphs algorithm nEncoding
    let err = readError result
    when (err /= 0) $ error "Encoding failed"
    getCodes


encodeGraphs :: EncodingType -> Maybe Int -> IO ErrorCode
encodeGraphs SingleLiteral      _       = singleLiteralEncoding
encodeGraphs Sequential         _       = sequentialEncoding
encodeGraphs Random         (Just a)    = randomEncoding        a
encodeGraphs Random             _       = randomEncoding        10
encodeGraphs Heuristic      (Just a)    = heuristicEncoding     a
encodeGraphs Heuristic          _       = heuristicEncoding     10
encodeGraphs Exhaustive     (Just a)    = exhaustiveEncoding    a
encodeGraphs Exhaustive         _       = exhaustiveEncoding    10

getCodes :: IO [CodeWithoutUnknowns]
getCodes = do
    nGraphs <- getNumGraphs
    bitLength <- getCodesLength
    readCodes nGraphs bitLength

readCodes :: Int -> Int -> IO [CodeWithoutUnknowns]
readCodes nPO bitLength = traverse (getCode bitLength) [0..nPO-1]

foreign import ccall unsafe "unload_graphs_codes"
    unloadGraphsAndCodes :: IO ErrorCode

foreign import ccall unsafe "load_graphs_codes"
    insertGraphsAndCodes :: CString -> CString -> IO ErrorCode

foreign import ccall unsafe "get_bit"
    getBit :: Int -> Int -> IO Int

foreign import ccall unsafe "get_codes_length"
    getCodesLength :: IO Int

foreign import ccall unsafe "get_n_graphs"
    getNumGraphs :: IO Int

foreign import ccall unsafe "single_literal_encoding"
    singleLiteralEncoding :: IO ErrorCode

foreign import ccall unsafe "sequential_encoding"
    sequentialEncoding :: IO ErrorCode

foreign import ccall unsafe "random_encoding"
    randomEncoding :: Int -> IO ErrorCode

foreign import ccall unsafe "heuristic_encoding"
    heuristicEncoding :: Int -> IO ErrorCode

foreign import ccall unsafe "exhaustive_encoding"
    exhaustiveEncoding :: Int -> IO ErrorCode

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

getCode :: Int -> Int -> IO CodeWithoutUnknowns
getCode bitLength poID = traverse (getCodeBit poID) [0..bitLength-1]

--encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
--encode = undefined

--overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
--overlay = undefined
