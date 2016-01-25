module Code (CodeWithUnknowns, CodeWithoutUnknowns) where

type BoolWithUnknowns = Maybe Bool
type Bit a = Maybe a

type CodeWithUnknowns = [Bit BoolWithUnknowns]

type CodeWithoutUnknowns = [Bit Bool]

known :: Bool -> Bit BoolWithUnknowns
known value = Just (Just value)

unused :: Bit BoolWithUnknowns
unused = Nothing

unknonwn :: Bit BoolWithUnknowns
unknonwn = Just Nothing
