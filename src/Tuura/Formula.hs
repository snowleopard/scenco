module Tuura.Formula (Formula, generateController, generateCPOG,
                      freeFormulae) where

import Foreign.C.String

abc = "abc"

data Formula = Formula String

getStringFormula :: Formula -> String
getStringFormula (Formula body) = body

createFormula :: String -> Formula
createFormula body = (Formula body)

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
