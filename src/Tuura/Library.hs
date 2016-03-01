module Tuura.Library (Library, libraryFile, loadLibrary) where

newtype Library = Library FilePath

libraryFile :: Library -> FilePath
libraryFile (Library file) = file

loadLibrary :: FilePath -> Library
loadLibrary = Library
