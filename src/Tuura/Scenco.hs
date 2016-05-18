module Tuura.Scenco ( Target (..), loadGraphsAndCodes, loadOnlyGraphs,
                      singleLiteralSearch, sequentialSearch, randomSearch,
                      heuristicSearch, exhaustiveSearch, mapVerilog,
                      unloadGraphs, scencoVersion ) where

import Tuura.Encode
import Tuura.Code
import Tuura.Synthesis
import Tuura.TechnologyMapping
import Tuura.Library
import Tuura.Formula
import Control.Monad

scencoVersion :: String
scencoVersion = "scenco v0.1"

data Target = CPOG | MICROCONTROLLER

loadGraphsAndCodes :: GraphFile -> CodeFile -> IO ([CodeWithUnknowns])
loadGraphsAndCodes graphFile codeFile = do
    loadGraphs graphFile
    c <- loadCodes (codeFilePath codeFile)
    encodingPreparation
    return c

loadOnlyGraphs :: GraphFile -> IO ()
loadOnlyGraphs graphFile = do
    loadGraphs graphFile
    encodingPreparation

loadGraphs :: GraphFile -> IO ()
loadGraphs graphs = do
    result <- setGraphs graphs
    check result "Load graphs"

loadCodes :: FilePath -> IO ([CodeWithUnknowns])
loadCodes ""        = do
    nGraphs <- getNumGraphs
    putStrLn ("Scenco will not set any constraints for the " ++ show nGraphs ++ " graphs.")
    result <- setCodes (CodeFile "")
    check result "Load codes"
    return (constraintFreeCodes nGraphs)
loadCodes codeFile = do
    let codes = CodeFile codeFile
    codeConstraints <- parseCustomCode codes
    result <- setCodes codes
    check result "Load codes"
    return codeConstraints

encodingPreparation :: IO ()
encodingPreparation = do
    result <- encodingAllocation
    check result "Encoding memory allocation"

sequentialSearch :: GraphFile -> Library -> Target -> IO (Formulae, [CodeWithoutUnknowns])
sequentialSearch graphsPath techLibPath target = do
    result <- encodeGraphs Sequential Nothing
    check result "Sequential encoding"
    codesSequential <- getCodes
    f <- synthesisTarget target graphsPath techLibPath codesSequential
    return (f, codesSequential)

singleLiteralSearch :: GraphFile -> Library -> Target -> IO (Formulae,[CodeWithoutUnknowns])
singleLiteralSearch graphsPath techLibPath target = do
    result <- encodeGraphs SingleLiteral Nothing
    check result "Single literal encoding"
    codeSingleLiteral <- getCodes
    f <- synthesisTarget target graphsPath techLibPath codeSingleLiteral
    return (f,codeSingleLiteral)

randomSearch :: GraphFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
randomSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Random (Just sol)
    putStr "Random encoding: "
    codesFinal `shouldMeet` codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

heuristicSearch :: GraphFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
heuristicSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Heuristic (Just sol)
    putStr "Heuristic encoding: "
    codesFinal `shouldMeet` codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

exhaustiveSearch :: GraphFile -> Library -> [CodeWithUnknowns] -> Int -> Target -> IO (Formulae,[CodeWithoutUnknowns])
exhaustiveSearch graphsPath techLibPath codeConstraints sol target = do
    codesFinal <- encode Exhaustive (Just sol)
    putStr "Exhaustive encoding: "
    codesFinal `shouldMeet` codeConstraints
    f <- synthesisTarget target graphsPath techLibPath codesFinal
    return (f,codesFinal)

synthesisTarget :: Target -> GraphFile -> Library -> [CodeWithoutUnknowns] -> IO Formulae
synthesisTarget MICROCONTROLLER = synthesiseController
synthesisTarget CPOG            = synthesiseCpog

unloadGraphs :: IO ()
unloadGraphs = do
    result <- unloadGraphsAndCodes
    check result "Unload graphs and codes"

synthesiseController :: GraphFile -> Library ->[CodeWithoutUnknowns] -> IO Formulae
synthesiseController graphs techLibPath codes = do
    formulae <- synthesiseControllerIO graphs codes
    areaEstimation techLibPath formulae
    return formulae

synthesiseCpog :: GraphFile -> Library ->[CodeWithoutUnknowns] -> IO Formulae
synthesiseCpog graphs techLib codes = do
    formulae <- synthesiseCpogIO graphs codes
    areaEstimation techLib formulae
    return formulae

areaEstimation :: Library -> Formulae -> IO ()
areaEstimation libFile formulae = do
    area <- estimateArea libFile formulae
    let size = parseArea area
    putStrLn $ "\tArea of the controller: " ++ show size

mapVerilog :: Formulae -> Library -> FilePath -> IO ()
mapVerilog formulae techLib verilogPath = do
    resultV <- writeVerilog techLib formulae verilogPath
    check (ErrorCode $ readErrVerilog resultV) "\tVerilog file generation"

check :: ErrorCode -> String -> IO ()
check (ErrorCode code) msg
    | code == 0 = putStrLn $ msg ++ ": OK"
    | otherwise = error $ msg ++ ": ERROR (code " ++ show code ++ ")"

shouldMeet :: [CodeWithoutUnknowns] -> [CodeWithUnknowns] -> IO ()
shouldMeet [] [] = putStrLn "Valid encoding"
shouldMeet xs [] = error $ "Extra codes found " ++ show xs
shouldMeet [] ys = error $ "Missing codes for " ++ show ys
shouldMeet (x:xs) (y:ys) = do
    let result = validate y x
    when (result /= Valid) . error $
        show result ++ ": " ++ show y ++ " => " ++ show x
    shouldMeet xs ys
