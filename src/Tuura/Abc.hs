module Tuura.Abc (abcCommand, abcCheck) where

import System.Directory

abcCommand :: FilePath
abcCommand = "abc"

abcCheck :: IO Bool
abcCheck = doesFileExist abcCommand
