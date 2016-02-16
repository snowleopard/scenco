module Tuura.Synthesis (synthesis, SynthesisType (..),
                        unloadController) where

import Tuura.Formula

data SynthesisType = Controller
                   | CPOG

--synthesis :: FilePath -> IO [Formula]
--synthesis espressoPath = do

synthesis :: FilePath -> SynthesisType -> IO Int
synthesis espressoPath Controller   = generateController espressoPath
synthesis espressoPath CPOG         = generateCPOG      espressoPath

unloadController :: IO ()
unloadController = freeFormulae
