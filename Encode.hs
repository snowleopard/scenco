module Encode (encode, overlay) where

import Code
import Graph

-- Guarantees:
-- 1) Turn unknowns to knowns:
--     Known True -> Known True
--     Known False -> Known False
--     Unused -> Unused
--     Unknown -> Known True or Known False
-- 2) The number codes should be the same -- give up on this
-- 3) No codes are conflicting

-- Possible reasons to fail:
-- 1) Input codes are conflicting
-- 2) Input codes are not feasible (not enough bits)
-- 3) Internal error: gurantees are not satisfied
encode :: [(Graph, CodeWithUnknowns)] -> Either String [CodeWithoutUnknowns]
encode = undefined

overlay :: [(Graph, CodeWithoutUnknowns)] -> Graph
overlay = undefined
