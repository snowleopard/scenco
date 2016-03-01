module Tuura.TechnologyMapping (Area, GateCount, parseArea,
                                estimateArea, writeVerilog, getArea, getGateCount
                                ) where

import Tuura.Formula
import Tuura.Library
import Tuura.Abc
import Foreign.C.String

--technologyMapping :: Library -> [Formula] -> Circuit
--technologyMapping = undefined

newtype Area      = Area Double
newtype GateCount = GateCount Int
newtype ErrorCode = ErrorCode Int

parseArea :: Area -> Double
parseArea (Area a) = a

estimateArea :: Library -> Formulae -> IO Area
estimateArea library formulae = do
    file <- newCString $ libraryFile library
    loadFormulae formulae
    abcC <- newCString abcCommand
    area <- mapAndGetAreaCircuit abcC file
    unloadFormulae
    return area

writeVerilog :: Library -> Formulae -> FilePath -> IO ErrorCode
writeVerilog library formulae verilogFile = do
    lFile <- newCString $ libraryFile library
    vFile <- newCString verilogFile
    abcC <- newCString abcCommand
    loadFormulae formulae
    result <- generateVerilog abcC lFile vFile
    unloadFormulae
    return result

foreign import ccall unsafe "map_and_get_area_circuit"
    mapAndGetAreaCircuit :: CString -> CString -> IO Area

foreign import ccall unsafe "generate_verilog"
    generateVerilog :: CString -> CString -> CString -> IO ErrorCode

foreign import ccall unsafe "get_area"
    getArea :: IO Area

foreign import ccall unsafe "get_gate_count"
    getGateCount :: IO GateCount
