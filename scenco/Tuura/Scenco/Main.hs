module Tuura.Scenco.Main (main) where

import Control.Monad

import Tuura.Scenco
import Tuura.Encode
import Tuura.Code
import Tuura.Scenco.Options
import Tuura.Abc
import Tuura.Library

main :: IO ()
main = do
    putStrLn scencoVersion

    abcCheck

    options <- getOptions
    let graphs       = optInput    options
        codes        = optCodes    options
        techLib      = optTechLib  options
        verilogFile  = optVerilog  options
        printVerilog = optPrintVer options
        encodingMode = optMode     options
        numberSol    = optNumSol   options
        microSynth   = optTarget   options

    libCheck techLib

    (bf, enc) <- case encodingMode of
        Sequential -> do
            loadOnlyGraphs graphs
            sequentialSearch graphs techLib microSynth
        SingleLiteral -> do
            loadOnlyGraphs graphs
            singleLiteralSearch graphs techLib microSynth
        Random -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            randomSearch graphs techLib codeConstraints numberSol microSynth
        Heuristic -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            heuristicSearch graphs techLib codeConstraints numberSol microSynth
        Exhaustive -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            exhaustiveSearch graphs techLib codeConstraints numberSol microSynth

    optOutput options (showEncoding enc)
    when printVerilog $ mapVerilog bf techLib verilogFile
    unloadGraphs
