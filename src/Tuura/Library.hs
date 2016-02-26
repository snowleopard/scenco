module Tuura.Library (Library, getLibraryPath, createLibrary) where

data Library = Library FilePath

getLibraryPath :: Library -> FilePath
getLibraryPath (Library file) = file

createLibrary :: FilePath -> Library
createLibrary file = (Library file)
