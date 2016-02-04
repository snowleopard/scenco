module Code (CodeWithUnknowns, CodeWithoutUnknowns, known, unused, used, unknown, Bit) where

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
