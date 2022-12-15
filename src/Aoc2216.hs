module Aoc2216 (aoc2216) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Name = String
type FlowRate = Int
type Node = (Name, FlowRate) -- open status modelled by two sets
type Edge = (Name, Name)
type Distance = Int
type DistanceTo = (Name, Distance)
type Distances = Map.Map Name [DistanceTo]
-- open nodes, closed nodes, directed edges, distance from key node to val nodes
-- TODO check if edges all biderectional or let it fail during runtime (if distance not in Distances)
data Graph = Graph [Node] [Edge] deriving (Eq, Show) 
data State = State [Node] [Node] Distances deriving (Eq, Show) -- open nodes, closed nodes, distance from key node to val nodes

-- Part a

-- Implement the graph to better see, how to optimize this.

translate :: [String] -> Graph
translate = foldr (joinGraph . translateLine) (Graph [] [])

translateLine :: String -> Graph
translateLine l = let (nodeName:restStr:_) = Split.splitOn " has flow rate=" $ drop 6 l
                      (flowRate:destNodesString:_) = let split = Split.splitOn "; tunnels lead to valves " restStr
                                                     in if length split == 1 then Split.splitOn "; tunnel leads to valve " restStr
                                                                             else split
                      edges = map (fixFst nodeName) $ Split.splitOn ", " destNodesString
                  in Graph [(nodeName, readInt flowRate)] edges

-- No check for duplicates
joinGraph :: Graph -> Graph -> Graph
joinGraph (Graph n1s e1s) (Graph n2s e2s) = Graph (n1s ++ n2s) (e1s ++ e2s) 

toState :: Graph -> State
toState (Graph nodes edges) = State [] nodes $ toDistances edges

toDistances :: [Edge] -> Distances -- TODO probably something akin to a transitive hull
toDistances edges = Map.empty

aoc2216 :: IO ()
aoc2216 = do 
    input <- readFile "./resources/input16test"
    -- input <- readFile "./resources/input16real"
    -- part a
    print $ translate $ splitIntoLines input


