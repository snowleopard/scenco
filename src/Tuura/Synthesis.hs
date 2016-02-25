module Tuura.Synthesis (synthesis, SynthesisType (..),
                        unloadController) where

import Tuura.Formula

data SynthesisType = Controller
                   | CPOG

--synthesis :: FilePath -> IO [Formula]
--synthesis espressoPath = do

synthesis :: FilePath -> SynthesisType -> IO Int
synthesis abcPath Controller   = generateController abcPath
synthesis abcPath CPOG         = generateCPOG      abcPath

unloadController :: IO ()
unloadController = freeFormulae
