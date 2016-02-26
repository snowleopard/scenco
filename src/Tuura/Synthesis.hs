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
