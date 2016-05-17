module Tuura.Abc (abcCommand, abcCheck) where

import System.Directory

abcCommand :: FilePath
abcCommand = "abc"

abcCheck :: IO ()
abcCheck = do
    maybePath <- findExecutable abcCommand
    case maybePath of
        Just _  -> return ()
        Nothing -> error $    "ABC synthesis tool not found.\n"
                           ++ "Read README file for the instructions on "
                           ++ "how to install and set ABC in the system path."
