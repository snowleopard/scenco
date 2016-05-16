module Tuura.Library (Library, libraryFile, loadLibrary, libCheck) where

import System.Directory

newtype Library = Library FilePath

libraryFile :: Library -> FilePath
libraryFile (Library file) = file

loadLibrary :: FilePath -> Library
loadLibrary = Library

libCheck :: Library -> IO Bool
libCheck lib = doesFileExist (libraryFile lib)
