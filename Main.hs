module Main (main) where

import Encode
import Synthesis
import TechnologyMapping
import Test

main = do
    testArm8
    testArm11
    testIntel7
    testIntel8
    testIntel9
