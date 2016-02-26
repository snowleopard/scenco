module Tuura.Synthesis (synthesis, SynthesisType (..),
                        unloadController) where

import Tuura.Formula

module Tuura.Synthesis (synthesis, SynthesisType (..),
                        unloadController) where

import Tuura.Formula

data SynthesisType = Controller
                   | CPOG

synthesis :: SynthesisType -> IO [Formula]
synthesis Controller   = generateController
synthesis CPOG         = generateCPOG

unloadController :: IO ()
unloadController = freeFormulae

import System.FilePath
import Foreign.C.String

abcCommand :: FilePath
abcCommand = "abc"

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

getFormula :: Int -> IO (Formula)
getFormula formulaID = do
    formulaCString <- getEquation formulaID
    formulaString <- peekCString formulaCString
    return $ createFormula formulaString

getFormulae :: Int -> IO [Formula]
getFormulae nFormulae = map getFormula [0..nFormulae-1]

generateController :: IO [Formula]
generateController = do
    nFormulae <- getNumFormulae
    abcC <- newCString abc
    generateFormulaeController abcC
    formulae <- getFormulae nFormulae
    return formulae

generateCPOG :: IO [Formula]
generateCPOG = do
    nFormulae <- getNumFormulae
    abcC <- newCString abc
    generateFormulaeCPOG abcC
    getFormula [0..nFormulae-1]

data SynthesisType = Controller
                   | CPOG

synthesis :: SynthesisType -> IO [Formula]
synthesis Controller   = generateController
synthesis CPOG         = generateCPOG

unloadController :: IO ()
unloadController = freeFormulae
