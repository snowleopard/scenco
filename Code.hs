module Code (
    CodeWithUnknowns, CodeWithoutUnknowns, Bit,
    known, unknown, used, unused, CodeValidation (..), validate
    ) where

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

validate :: CodeWithUnknowns -> CodeWithoutUnknowns -> CodeValidation
validate []     []     = Valid
validate []     _      = LengthMismatch
validate _      []     = LengthMismatch
validate (a:as) (b:bs)
    | a == unused      = (b /= unused    ) `thenError` UnusedBitRemoved
    | a == unknown     = (b == unused    ) `thenError` UnusedBitAdded
    | a == known True  = (b /= used True ) `thenError` KnownBitChanged
    | a == known False = (b /= used False) `thenError` KnownBitChanged
  where
    condition `thenError` result = if condition then result else validate as bs
