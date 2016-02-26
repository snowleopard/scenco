module Tuura.Formula (Formula, printFormula, parseFormula) where

newtype Formula = Formula String

printFormula :: Formula -> String
printFormula (Formula s) = s

parseFormula :: String -> Formula
parseFormula = Formula
