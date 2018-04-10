module Poets.Formats.GraphAlgebra where

import Data.List

import Poets.Critic.Types

convertToAlgebra :: Graph -> String
convertToAlgebra g = intercalate " + " sequences
  where
    es = getEdgeInstances g
    sequences = map (\e -> inNode e ++ " -> " ++ outNode e) es

-- getEdgeInstances :: Graph -> [EdgeInstance]
-- getEdgeInstances g = eis
--   where
--   	gi = graphInstance g
--   	eis = edgeInstances gi
