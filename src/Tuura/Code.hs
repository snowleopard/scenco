module Tuura.Code (
    CodeWithUnknowns, CodeWithoutUnknowns, Bit, CodesFile, codesFilepath, getCodesFile,
    known, unknown, used, unused, CodeValidation (..), validate, parseCustomCode,
    showEncoding, constraintFreeCodes ) where

type BoolWithUnknowns = Maybe Bool
type Bit a = Maybe a
type CodeWithUnknowns = [Bit BoolWithUnknowns]
type CodeWithoutUnknowns = [Bit Bool]

newtype CodesFile = CodesFile FilePath

codesFilepath :: CodesFile -> FilePath
codesFilepath (CodesFile file) = file

getCodesFile :: FilePath -> CodesFile
getCodesFile = CodesFile

known :: Bool -> Bit BoolWithUnknowns
known value = Just (Just value)

unused :: Bit a
unused = Nothing

used :: Bool -> Bit Bool
used value = Just value

unknown :: Bit BoolWithUnknowns
unknown = Just Nothing

data CodeValidation = Valid
                    | LengthMismatch
                    | UnusedBitRemoved
                    | UnusedBitAdded
                    | KnownBitChanged
                    deriving (Show, Eq)

validate :: CodeWithUnknowns -> CodeWithoutUnknowns -> CodeValidation
validate []     []     = Valid
validate []     _      = LengthMismatch
validate _      []     = LengthMismatch
validate (a:as) (b:bs)
    | a == unused      = (b /= unused    ) `thenError` UnusedBitRemoved
    | a == unknown     = (b == unused    ) `thenError` UnusedBitAdded
    | a == known True  = (b /= used True ) `thenError` KnownBitChanged
    | a == known False = (b /= used False) `thenError` KnownBitChanged
    | otherwise        = undefined -- Never reached, but otherwise GHC complains
  where
    condition `thenError` result = if condition then result else validate as bs

parseCustomCode :: CodesFile -> IO ([CodeWithUnknowns])
parseCustomCode codesPath = do
    let codesF = codesFilepath codesPath
    contents <- lines <$> readFile codesF
    let codes = readCodes contents
    return codes

-- not sure how to remove the warning generate by the next line
constraintFreeCodes :: Int -> [CodeWithUnknowns]
constraintFreeCodes n = getConstFreeCodes (ceiling (logBase (2.0) (fromIntegral n))) n

getConstFreeCodes :: Int -> Int -> [CodeWithUnknowns]
getConstFreeCodes _ 0 = []
getConstFreeCodes b n = (getConstFreeCode b) : (getConstFreeCodes b (n-1))

getConstFreeCode :: Int -> CodeWithUnknowns
getConstFreeCode 0 = []
getConstFreeCode b = (Just Nothing) : (getConstFreeCode (b-1))

readBit :: Char -> Bit BoolWithUnknowns
readBit x
    | x == '0'       = known False
    | x == '1'       = known True
    | x == 'X'       = unknown
    | x == '-'       = unused
    | otherwise      = error $ "readBit: character '" ++ show x ++ "' is not recognised."

readCode :: String -> CodeWithUnknowns
readCode = map readBit

readCodes :: [String] -> [CodeWithUnknowns]
readCodes = map readCode

showEncoding :: [CodeWithoutUnknowns] -> String
showEncoding []         = ""
showEncoding (x : xs)   = showCode x ++ "\n" ++ showEncoding xs

showCode :: [Bit Bool] -> String
showCode []                   = ""
showCode ((Just False) : xs)  = "0" ++ showCode xs
showCode ((Just True)  : xs)  = "1" ++ showCode xs
showCode (Nothing      : xs)  = "-" ++ showCode xs
