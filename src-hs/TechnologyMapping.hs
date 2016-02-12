module TechnologyMapping (technologyMapping, Area (..), estimateArea) where

import Library
import Formula
import Circuit

technologyMapping :: Library -> [Formula] -> Circuit
technologyMapping = undefined

data Area = Area Double

estimateArea :: Library -> [Formula] -> Area
estimateArea = undefined
