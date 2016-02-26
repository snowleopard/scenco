module Tuura.TechnologyMapping (technologyMapping, Area, GateCount, 
                                estimateArea, writeVerilog) where

import Foreign.C.String

import Tuura.Circuit
import Tuura.Formula
import Tuura.Library

type Area      = Double
type GateCount = Int
type ErrorCode = Int

-- TODO: Move to a separate file
abcCommand :: FilePath
abcCommand = "abc"

foreign import ccall unsafe "map_and_get_area_circuit"
    mapAndGetAreaCircuit :: CString -> CString -> IO Area

foreign import ccall unsafe "generate_verilog"
    generateVerilog :: CString -> CString -> CString -> IO ErrorCode

technologyMapping :: Library -> [Formula] -> Circuit
technologyMapping = undefined

-- TODO: Add missing parameters
estimateArea :: Library -> IO Area
estimateArea library = do
    file <- newCString $ libraryFile library
    mapAndGetAreaCircuit file

-- TODO: Add missing parameters
writeVerilog :: Library -> FilePath -> IO ErrorCode
writeVerilog library verilogFile = do
    lFile <- newCString $ libraryFile library
    vFile <- newCString verilogFile
    generateVerilog abcCommand lFile vFile
