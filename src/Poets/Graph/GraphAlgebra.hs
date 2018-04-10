module Poets.Graph.GraphAlgebra where

import Data.List

import Poets.Critic.Types

convertToAlgebra :: Graph -> String
convertToAlgebra g = intercalate " + " sequences
  where
    es = getEdgeInstances g
    sequences = map (\e -> i e ++ " -> " ++ o e) es
    i = deviceInstanceID . inNode
    o = deviceInstanceID . outNode
