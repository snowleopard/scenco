module Tuura.TechnologyMapping (technologyMapping, Area (..), estimateArea,
                                writeVerilog) where

import Foreign.C.String

import Tuura.Circuit
import Tuura.Formula
import Tuura.Library

data Area = Area Double

foreign import ccall unsafe "map_and_get_area_circuit"
    mapAndGetAreaCircuit :: CString -> CString -> IO (Double)

foreign import ccall unsafe "generate_verilog"
    generateVerilog :: CString -> CString -> CString -> IO (Int)

technologyMapping :: Library -> [Formula] -> Circuit
technologyMapping = undefined

estimateArea :: FilePath -> FilePath -> IO (Double)
estimateArea abcPath techLibPath = do
    abc <- newCString abcPath
    techLib <- newCString techLibPath
    mapAndGetAreaCircuit abc techLib

writeVerilog :: FilePath -> FilePath -> FilePath -> IO (Int)
writeVerilog abcPath techLibPath verilogPath = do
    abc <- newCString abcPath
    techLib <- newCString techLibPath
    vFile <- newCString verilogPath
    generateVerilog abc techLib vFile
