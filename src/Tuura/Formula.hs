module Tuura.Formula (Formulae, Input, Output, Equation,
                      parseFormula, getFormulae, loadFormulae, unloadFormulae, selectOutputs
                      ) where

import Foreign.C.String

newtype Input    = Input String
newtype Output   = Output String
newtype Equation = Equation String

data Formulae    = Formulae
    {
        inputs    :: [Input]
    ,   outputs   :: [Output]
    ,   equations :: [Equation]
    }

parseFormula :: [Input] -> [Output] -> [Equation] -> Formulae
parseFormula = Formulae

getFormulae :: IO Formulae
getFormulae = do
    nInputs <- getNumInputs
    ins <- getInputs nInputs
    nOutputs <- getNumOutputs
    outs <- getOutputs nOutputs
    nEquations <- getNumEquations
    eqs <- getEquations nEquations
    let formula = parseFormula ins outs eqs
    return formula

loadFormulae :: Formulae -> IO ()
loadFormulae formulae = do
    inputCStrings <- convertInputs $ selectInputs formulae
    outputCStrings <- convertOutputs $ selectOutputs formulae
    equationCStrings <- convertEquations $ selectEquations formulae
    loadInputs inputCStrings
    loadOutputs outputCStrings
    loadEquations equationCStrings

foreign import ccall unsafe "free_formulae"
    unloadFormulae :: IO ()

printInput :: Input -> String
printInput (Input s) = s

loadInput :: String -> Input
loadInput = Input

printOutput :: Output -> String
printOutput (Output s) = s

loadOutput :: String -> Output
loadOutput = Output

printEquation :: Equation -> String
printEquation (Equation s) = s

loadEquation :: String -> Equation
loadEquation = Equation

selectInputs :: Formulae -> [Input]
selectInputs  = inputs

selectOutputs :: Formulae -> [Output]
selectOutputs = outputs

selectEquations :: Formulae -> [Equation]
selectEquations = equations

getInputs :: Int -> IO [Input]
getInputs nInputs = traverse getInput [0..nInputs-1]

getInput :: Int -> IO Input
getInput inputID = do
    inputCString <- get_input inputID
    inputString <- peekCString inputCString
    let input = loadInput inputString
    return input

getOutputs :: Int -> IO [Output]
getOutputs nOutputs = traverse getOutput [0..nOutputs-1]

getOutput :: Int -> IO Output
getOutput outputID = do
    outputCString <- get_output outputID
    outputString <- peekCString outputCString
    let output = loadOutput outputString
    return output

getEquations :: Int -> IO [Equation]
getEquations nEquations = traverse getEquation [0..nEquations-1]

getEquation :: Int -> IO Equation
getEquation formulaID = do
    equationCString <- get_equation formulaID
    equationString <- peekCString equationCString
    let equation = loadEquation equationString
    return equation

convertInputs :: [Input] -> IO [CString]
convertInputs is = mapM newCString $ (map printInput is)

convertOutputs :: [Output] -> IO [CString]
convertOutputs os = mapM newCString $ (map printOutput os)

convertEquations :: [Equation] -> IO [CString]
convertEquations es = mapM newCString $ (map printEquation es)

loadInputs :: [CString] -> IO ()
loadInputs is = mapM_ pushInput is

loadOutputs :: [CString] -> IO ()
loadOutputs os = mapM_ pushOutput os

loadEquations :: [CString] -> IO ()
loadEquations es = mapM_ pushEquation es

foreign import ccall unsafe "get_input"
    get_input :: Int -> IO CString

foreign import ccall unsafe "push_input"
    pushInput :: CString -> IO ()

foreign import ccall unsafe "get_num_inputs"
    getNumInputs :: IO Int

foreign import ccall unsafe "get_output"
    get_output :: Int -> IO CString

foreign import ccall unsafe "push_output"
    pushOutput :: CString -> IO ()

foreign import ccall unsafe "get_num_outputs"
    getNumOutputs :: IO Int

foreign import ccall unsafe "get_equation"
    get_equation :: Int -> IO CString

foreign import ccall unsafe "push_equation"
    pushEquation :: CString -> IO ()

foreign import ccall unsafe "get_num_equations"
    getNumEquations :: IO Int
