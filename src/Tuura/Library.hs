module Tuura.Library (Library, libraryFile, loadLibrary, libCheck) where

import System.Directory
import Control.Monad

newtype Library = Library FilePath

libraryFile :: Library -> FilePath
libraryFile (Library file) = file

loadLibrary :: FilePath -> Library
loadLibrary = Library

libCheck :: Library -> IO ()
libCheck lib = do
    libPresent <- isLibPresent lib
    when (libPresent == False) . putStrLn $ "Gate library " ++ show (libraryFile lib) ++ " not found."

isLibPresent :: Library -> IO Bool
isLibPresent lib = doesFileExist (libraryFile lib)
