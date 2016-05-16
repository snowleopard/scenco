module Tuura.Scenco.Options (Options(..), getOptions) where

import Tuura.Scenco
import Tuura.Encode
import Tuura.Library
import Tuura.Code
import Tuura.Graph

import Data.Foldable (foldlM)
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

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
    [ Option ['m'] ["microcontroller"]
      (NoArg (\opts -> return opts { optTarget = MICROCONTROLLER }))
      "Synthesise controller targeting the microcontroller optimisation"
    , Option ['e'] ["encoding"]
      (ReqArg (\mode opts -> return opts { optMode = getMode mode }) "SEARCH-TYPE")
      "Available: sequential - single-literal - random - heuristic - exhaustive"
    , Option ['c'] ["constraints"]
      (ReqArg (\f opts -> return opts { optCodes = getCodesFile f }) "FILE-CONSTRAINTS")
      "Set encoding constraints"
    , Option ['n'] ["number-solutions"]
      (ReqArg (\n opts -> return opts { optNumSol = read n }) "NUMBER")
      "Set number of solutions to generate (higher is better/slower)"
    , Option ['o'] ["output"]
      (ReqArg (\f opts -> return opts { optOutput = writeFile f }) "FILE-ENCODING")
      "Write encoding into a file"
    , Option ['v'] ["verilog"]
      (ReqArg (\f opts -> return opts { optPrintVer = True
                                      , optVerilog  = f }) "FILE-VERILOG")
      "Output the controller into a verilog file"
    , Option ['h'] ["help"]
      (NoArg (\opts -> return opts { optHelp = True }))
      "Show this help message"
    ]

getOptions :: IO Options
getOptions = do
    argv   <- getArgs
    result <- case getOpt Permute options argv of
        (opts, [] , []  )   -> foldlM (flip id) defaultOptions opts
        (_, [_], []  )      -> ioError $ userError ("First two arguments must be "
                               ++ "[input file] [tech lib]")
        (opts, [f,t], []  ) -> foldlM (flip id)
                               defaultOptions { optInput   = loadGraph   f
                                              , optTechLib = loadLibrary t } opts
        (_   , _  , []  )   -> ioError $ userError "Multiple input files"
        (_   , _  , errs)   -> ioError . userError $ concat errs
    when (optHelp result) $ do
        progName <- getProgName
        let header = "Usage: " ++ progName ++ " [input file] [tech lib] [OPTIONS...]"
            helpMessage = usageInfo header options
        putStrLn helpMessage
        exitSuccess
    return result
