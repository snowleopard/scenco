module Tuura.Scenco.Options (Options(..), getOptions) where

import Data.Foldable
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

import Tuura.Scenco
import Tuura.Encode
import Tuura.Library
import Tuura.Code
import Tuura.Graph

data Options = Options
    { optHelp     :: Bool
    , optPrintVer :: Bool
    , optTarget   :: Target
    , optMode     :: EncodingType
    , optInput    :: GraphsFile
    , optCodes    :: CodesFile
    , optVerilog  :: FilePath
    , optTechLib  :: Library
    , optNumSol   :: Int
    , optOutput   :: String -> IO () }

defaultOptions :: Options
defaultOptions    = Options
    { optHelp     = False
    , optPrintVer = False
    , optTarget   = CPOG
    , optMode     = Sequential
    , optInput    = loadGraph ""
    , optCodes    = getCodesFile ""
    , optVerilog  = "cpog.v"
    , optTechLib  = loadLibrary ""
    , optOutput   = putStr
    , optNumSol   = 10 }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['e'] ["encoding"]
      (ReqArg (\mode opts -> return opts { optMode = getMode mode }) "METHOD")
      "Encoding method: sequential (default), single-literal, random, heuristic, exhaustive."

    , Option ['c'] ["constraints"]
      (ReqArg (\f opts -> return opts { optCodes = getCodesFile f }) "FILE")
      "Encoding constraints"

    , Option ['n'] ["n-solutions"]
      (ReqArg (\n opts -> return opts { optNumSol = read n }) "INTEGER")
      "Number of solutions to generate (higher is better/slower)"

    , Option ['o'] ["output"]
      (ReqArg (\f opts -> return opts { optOutput = writeFile f }) "FILE")
      "Output file"

    , Option ['m'] ["microcontroller"]
      (NoArg (\opts -> return opts { optTarget = MICROCONTROLLER }))
      "Optimise the resulting microcontroller instead of graph family"

    , Option ['v'] ["verilog"]
      (ReqArg (\f opts -> return opts { optPrintVer = True
                                      , optVerilog  = f }) "FILE")
      "Write the microcontroller into a Verilog file"

    , Option ['h'] ["help"]
      (NoArg (\opts -> return opts { optHelp = True }))
      "Show this help message"
    ]

getOptions :: IO Options
getOptions = do
    argv   <- getArgs
    result <- case getOpt Permute options argv of
        (opts, []    , []) -> foldlM (flip id) defaultOptions opts
        (_   , [_]   , []) -> ioError $ userError
                              "First two arguments must be [input file] [tech lib]"
        (opts, [f, t], []) -> foldlM (flip id)
                              defaultOptions { optInput   = loadGraph   f
                                             , optTechLib = loadLibrary t } opts
        (_   , _     , []) -> ioError $ userError "Multiple input files"
        (_   , _     , es) -> ioError . userError $ concat es
    when (optHelp result) $ do
        progName <- getProgName
        let header = "Usage: " ++ progName ++ " [input file] [tech lib] [OPTIONS...]"
            helpMessage = usageInfo header options
        putStrLn helpMessage
        exitSuccess
    return result
