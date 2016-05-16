module Tuura.Abc (abcCommand, abcCheck) where

import System.Directory
import Control.Monad

abcCommand :: FilePath
abcCommand = "abc"

abcCheck :: IO ()
abcCheck = do
    abcInstalled <- isAbcPresent
    when (abcInstalled == False) . error $ ("ABC synthesis tool not found.\n"
                                         ++ "Read README file for the instructions on "
                                         ++ "how to install and set ABC in the system path.")

isAbcPresent :: IO Bool
isAbcPresent = doesFileExist abcCommand
