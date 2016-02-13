module Tuura.TechnologyMapping (technologyMapping, Area (..), estimateArea) where

import Tuura.Circuit
import Tuura.Formula
import Tuura.Library

technologyMapping :: Library -> [Formula] -> Circuit
technologyMapping = undefined

data Area = Area Double

estimateArea :: Library -> [Formula] -> Area
estimateArea = undefined
