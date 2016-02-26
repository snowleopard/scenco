module Tuura.TechnologyMapping (technologyMapping, Area, getArea, setArea,
                                estimateArea, writeVerilog) where

import Foreign.C.String

import Tuura.Circuit
import Tuura.Formula
import Tuura.Library

data Area = Area Double

getArea :: Area -> Double
getSize (area size) = size

setArea :: Double -> Area
setSize size = (area size)

foreign import ccall unsafe "map_and_get_area_circuit"
    mapAndGetAreaCircuit :: CString -> CString -> IO (Double)

foreign import ccall unsafe "generate_verilog"
    generateVerilog :: CString -> CString -> CString -> IO (Int)

--technologyMapping :: Library -> [Formula] -> Circuit
--technologyMapping = undefined

estimateArea :: Library -> IO (Area)
estimateArea techLibPath = do
    library <- newCString $ (getLibraryPath techLib)
    size <- mapAndGetAreaCircuit library
    setArea size

writeVerilog :: Library -> FilePath -> IO (Int)
writeVerilog techLib verilogPath = do
    library <- newCString $ (getLibraryPath techLib)
    vFile <- newCString verilogPath
    generateVerilog abc library vFile
