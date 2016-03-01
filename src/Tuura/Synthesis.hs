module Tuura.Synthesis (synthesiseControllerIO, synthesiseCpogIO) where

import Tuura.Formula
import Tuura.Abc
import Tuura.Code
import Tuura.Graph
import Foreign.C.String
import Control.Monad

--synthesise :: [(Graph, CodeWithoutUnknowns)] -> [Formula]
--synthesise = undefined

synthesiseControllerIO :: GraphsFile -> [CodeWithoutUnknowns] -> IO Formulae
synthesiseControllerIO _ _ = do
    abcC <- newCString abcCommand
    errorCode <- generateController abcC
    when (errorCode /= 0) $ error "Controller synthesis failed"
    f <- getFormulae
    unloadFormulae
    return f

synthesiseCpogIO :: GraphsFile -> [CodeWithoutUnknowns] -> IO Formulae
synthesiseCpogIO _ _ = do
    abcC <- newCString abcCommand
    errorCode <- generateCPOG abcC
    when (errorCode /= 0) $ error "CPOG synthesis failed"
    f <- getFormulae
    unloadFormulae
    return f

foreign import ccall unsafe "get_controller_formulae"
    generateController :: CString -> IO Int

foreign import ccall unsafe "get_CPOG_formulae"
    generateCPOG :: CString -> IO Int
