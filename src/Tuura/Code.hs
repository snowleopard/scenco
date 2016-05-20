module Tuura.Code (
    CodeWithUnknowns, CodeWithoutUnknowns, Bit, known, unknown, used, unused,
    CodeValidation (..), CodeFile (..), validate, parseCustomCode, showEncoding,
    constraintFreeCodes
    ) where

import Data.Bits

type BoolWithUnknowns = Maybe Bool
type Bit a = Maybe a
type CodeWithUnknowns = [Bit BoolWithUnknowns]
type CodeWithoutUnknowns = [Bit Bool]

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

newtype CodeFile = CodeFile { codeFilePath :: FilePath }

parseCustomCode :: CodeFile -> IO ([CodeWithUnknowns])
parseCustomCode codePath = do
    contents <- lines <$> readFile (codeFilePath codePath)
    let codes = readCodes contents
    return codes

constraintFreeCodes :: Int -> [CodeWithUnknowns]
constraintFreeCodes num = unknowns bits num
  where
    bits = finiteBitSize num - countLeadingZeros (num - 1)

unknowns :: Int -> Int -> [CodeWithUnknowns]
unknowns bits num = replicate num (replicate bits unknown)

readBit :: Char -> Bit BoolWithUnknowns
readBit x
    | x == '0'  = known False
    | x == '1'  = known True
    | x == 'X'  = unknown
    | x == '-'  = unused
    | otherwise = error $ "readBit: character '" ++ show x ++ "' is not recognised."

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
