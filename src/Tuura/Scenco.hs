module Tuura.Scenco ( Target (..), loadGraphsAndCodes, loadOnlyGraphs,
                      singleLiteralSearch, sequentialSearch, randomSearch,
                      heuristicSearch, exhaustiveSearch, mapVerilog,
                      unloadGraphs, scencoVersion ) where

import Tuura.Encode
import Tuura.Code
import Tuura.Synthesis
import Tuura.TechnologyMapping
import Tuura.Graph
import Tuura.Library
import Tuura.Formula
import Control.Monad

scencoVersion :: String
scencoVersion = "Scenco v0.1"

data Target = CPOG | MICROCONTROLLER

loadGraphsAndCodes :: GraphsFile -> CodesFile -> IO ([CodeWithUnknowns])
loadGraphsAndCodes cpogFile codesFile = do
    loadGraphs cpogFile
    c <- loadCodes (codesFilepath codesFile)
    encodingPreparation
    return c

loadOnlyGraphs :: GraphsFile -> IO ()
loadOnlyGraphs cpogFile = do
    loadGraphs cpogFile
    encodingPreparation

loadGraphs :: GraphsFile -> IO ()
loadGraphs graphs = do
    result <- setGraphs graphs
    let err = readError result
    check err "Graphs loaded" "Error loading graphs"

loadCodes :: FilePath -> IO ([CodeWithUnknowns])
loadCodes ""        = do
    nGraphs <- getNumGraphs
    putStrLn ("Scenco will not set any constraints for the " ++ show nGraphs ++ " graphs.")
    result <- setCodes (getCodesFile "")
    let err = readError result
    check err "Codes loaded" "Error loading codes"
    return (constraintFreeCodes nGraphs)
loadCodes codesFile = do
    let codes = getCodesFile codesFile
    codeConstraints <- parseCustomCode codes
    result <- setCodes codes
    let err = readError result
    check err "Codes loaded" "Error loading codes"
    return codeConstraints

encodingPreparation :: IO ()
encodingPreparation = do
    result <- encodingAllocation
    let err = readError result
    check err "Scenco is ready to encode the graphs" "Error preparing Scenco for encoding"

sequentialSearch :: GraphsFile -> Library -> Target -> IO (Formulae,[CodeWithoutUnknowns])
sequentialSearch graphsPath techLibPath target = do
    result <- encodeGraphs Sequential Nothing
    let err = readError result
    check err "Sequential encoding: OK" "Sequential encoding: ERROR"
    codesSequential <- getCodes
    f <- synthesisTarget target graphsPath techLibPath codesSequential
    return (f, codesSequential)

singleLiteralSearch :: GraphsFile -> Library -> Target -> IO (Formulae,[CodeWithoutUnknowns])
singleLiteralSearch graphsPath techLibPath target = do
    result <- encodeGraphs SingleLiteral Nothing
    let err = readError result
    check err "Single literal encoding: OK" "Single literal encoding: ERROR"
    codeSingleLiteral <- getCodes
    f <- synthesisTarget target graphsPath techLibPath codeSingleLiteral
    return (f,codeSingleLiteral)

randomSearch :: GraphsFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
randomSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Random (Just sol)
    putStr "Random encoding: "
    shouldMeet codesFinal codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

heuristicSearch :: GraphsFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
heuristicSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Heuristic (Just sol)
    putStr "Heuristic encoding: "
    shouldMeet codesFinal codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

exhaustiveSearch :: GraphsFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
exhaustiveSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Exhaustive (Just sol)
    putStr "Exhaustive encoding: "
    shouldMeet codesFinal codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

synthesisTarget :: Target -> GraphsFile -> Library -> [CodeWithoutUnknowns] -> IO Formulae
synthesisTarget MICROCONTROLLER = synthesiseController
synthesisTarget CPOG            = synthesiseCpog

unloadGraphs :: IO ()
unloadGraphs = do
    result <- unloadGraphsAndCodes
    let err = readError result
    check err "Graphs and codes unloaded" "Error unloading graphs"

synthesiseController :: GraphsFile -> Library ->[CodeWithoutUnknowns] -> IO Formulae
synthesiseController graphs techLibPath codes = do
    formulae <- synthesiseControllerIO graphs codes
    areaEstimation techLibPath formulae
    return formulae

synthesiseCpog :: GraphsFile -> Library ->[CodeWithoutUnknowns] -> IO Formulae
synthesiseCpog graphs techLib codes = do
    formulae <- synthesiseCpogIO graphs codes
    areaEstimation techLib formulae
    return formulae

areaEstimation :: Library -> Formulae -> IO ()
areaEstimation libFile formulae = do
    area <- estimateArea libFile formulae
    let size = parseArea area
    putStrLn ("\tArea of the controller: " ++ show size)

mapVerilog :: Formulae -> Library -> FilePath -> IO ()
mapVerilog formulae techLib verilogPath = do
    resultV <- writeVerilog techLib formulae verilogPath
    let err = readErrVerilog resultV
    check err "\tVerilog file generation: OK" "Verilog file generation: ERROR"

check :: Int -> String -> String -> IO ()
check result msgOk msgError
    | result == 0 = putStrLn msgOk
    | otherwise   = error $ msgError ++ " (code " ++ show result ++ ")"

shouldMeet :: [CodeWithoutUnknowns] -> [CodeWithUnknowns] -> IO ()
shouldMeet [] [] = putStrLn "Valid encoding"
shouldMeet xs [] = error $ "Extra codes found " ++ show xs
shouldMeet [] ys = error $ "Missing codes for " ++ show ys
shouldMeet (x:xs) (y:ys) = do
    let result = validate y x
    when (result /= Valid) . error $
        show result ++ ": " ++ show y ++ " => " ++ show x
    shouldMeet xs ys
