module Tuura.Synthesis (synthesise, encodeAndSynthesiseIO) where

import Tuura.Formula

import System.FilePath
import Foreign.C.String

abcCommand :: FilePath
abcCommand = "abc"

synthesise :: [(Graph, CodeWithoutUnknowns)] -> [Formula]
synthesise = undefined

encodeAndSynthesiseIO :: FilePath -> FilePath -> IO [Formula]
encodeAndSynthesiseIO = do
    nFormulae <- getNumFormulae
    abcC <- newCString abc
    errorCode <- generateFormulaeController abcC
    when (errorCode /= 0) $ error "Error message"
    result <- getFormulae nFormulae
    unloadController
    return result

unloadController :: IO ()
unloadController = freeFormulae

foreign import ccall unsafe "get_controller_formulae"
    generateFormulaeController :: CString -> IO Int

foreign import ccall unsafe "get_CPOG_formulae"
    generateFormulaeCPOG :: CString -> IO Int

foreign import ccall unsafe "get_equation"
    getEquation :: Int -> IO CString

foreign import ccall unsafe "get_num_equations"
    getNumFormulae :: IO Int

foreign import ccall unsafe "free_formulae"
    freeFormulae :: IO ()

getFormula :: Int -> IO Formula
getFormula formulaID = do
    formulaCString <- getEquation formulaID
    formulaString <- peekCString formulaCString
    return $ parseFormula formulaString

getFormulae :: Int -> IO [Formula]
getFormulae nFormulae = traverse getFormula [0..nFormulae-1]
