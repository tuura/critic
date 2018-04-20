module Poets.Graph.GraphAlgebra where

import Data.List

import Poets.Critic.Types

convertToAlgebra :: Graph -> String
convertToAlgebra g = getGraphAlgebra $ getEdgeInstances g

getGraphAlgebra :: [EdgeInstance] -> String
getGraphAlgebra es = intercalate " + " sequences
  where
    sequences = map (\e -> i e ++ " -> " ++ o e) es
    i = deviceInstanceID . inNode
    o = deviceInstanceID . outNode
