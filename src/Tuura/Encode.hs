module Tuura.Encode (
    setGraphs, setCodes, encode, encodeGraphs, getCodes, unloadGraphsAndCodes,
    GraphFile (..), EncodingType (..), ErrorCode (..), encodingAllocation,
    getMode, getNumGraphs
    ) where

import Tuura.Code
import Foreign.C.String
import Control.Monad
import Data.Char

newtype GraphFile = GraphFile { graphFilePath :: FilePath }
newtype ErrorCode = ErrorCode { getErrorCode  :: Int }

data EncodingType = SingleLiteral
                  | Sequential
                  | Random
                  | Heuristic
                  | Exhaustive

setGraphs :: GraphFile -> IO ErrorCode
setGraphs graphsPath = do
    let graphsF = graphFilePath graphsPath
    graphs      <- newCString graphsF
    insertGraphs graphs

setCodes :: CodeFile -> IO ErrorCode
setCodes codePath = do
    codesConstraints <- newCString $ codeFilePath codePath
    insertCodes codesConstraints

encode :: EncodingType -> Maybe Int -> IO [CodeWithoutUnknowns]
encode algorithm nEncoding = do
    result <- encodeGraphs algorithm nEncoding
    let err = getErrorCode result
    when (err /= 0) $ error "Encoding failed"
    getCodes

getMode :: String -> EncodingType
getMode s = decodeMode (map toLower s)

decodeMode :: String -> EncodingType
decodeMode "sequential"     = Sequential
decodeMode "single-literal" = SingleLiteral
decodeMode "random"         = Random
decodeMode "heuristic"      = Heuristic
decodeMode "exhaustive"     = Exhaustive
decodeMode s                = error $ s ++ " approach not recognised."

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

foreign import ccall unsafe "load_graphs"
    insertGraphs :: CString -> IO ErrorCode

foreign import ccall unsafe "load_codes"
    insertCodes :: CString -> IO ErrorCode

foreign import ccall unsafe "encoding_vars_alloc"
    encodingAllocation :: IO ErrorCode

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
