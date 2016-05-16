module Tuura.Scenco.Main (main) where

import Tuura.Scenco
import Tuura.Encode
import Tuura.Code
import Tuura.Scenco.Options

import Control.Monad

main :: IO ()
main = do
    putStrLn scencoVersion

    options <- getOptions

    -- define variables used
    let graphs       = (optInput options)
        codes        = (optCodes options)
        techLib      = (optTechLib options)
        verilogFile  = (optVerilog options)
        printVerilog = (optPrintVer options)
        encodingMode = (optMode options)
        numberSol    = (optNumSol options)
        microSynth   = (optTarget options)

    -- run SCENCO
    case (encodingMode) of
        Sequential -> do
            loadOnlyGraphs graphs
            (bf,enc) <- sequentialSearch graphs techLib microSynth
            optOutput options (showEncoding enc)
            when (printVerilog) $ mapVerilog bf techLib verilogFile
        SingleLiteral -> do
            loadOnlyGraphs graphs
            (bf,enc) <- singleLiteralSearch graphs techLib microSynth
            optOutput options (showEncoding enc)
            when (printVerilog) $ mapVerilog bf techLib verilogFile
        Random -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            (bf,enc) <- randomSearch graphs techLib codeConstraints numberSol microSynth
            optOutput options (showEncoding enc)
            when (printVerilog) $ mapVerilog bf techLib verilogFile
        Heuristic -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            (bf,enc) <- heuristicSearch graphs techLib codeConstraints numberSol microSynth
            optOutput options (showEncoding enc)
            when (printVerilog) $ mapVerilog bf techLib verilogFile
        Exhaustive -> do
            codeConstraints <- loadGraphsAndCodes graphs codes
            (bf,enc) <- exhaustiveSearch graphs techLib codeConstraints numberSol microSynth
            optOutput options (showEncoding enc)
            when (printVerilog) $ mapVerilog bf techLib verilogFile
    unloadGraphs
