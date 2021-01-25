-- Ordering for greedy colouring of chordal graphs.
module X86_64.RegisterAllocation.PerfectEliminationOrdering (perfectEliminationOrdering) where

import           Data.List
import qualified Data.Map                                    as Map
import qualified Data.Set                                    as Set
import           X86_64.RegisterAllocation.InterferenceGraph

-- Get the perfect elimination ordering of an interference graph,
-- assuming it is chordal.
perfectEliminationOrdering :: InterferenceGraph -> [String]
perfectEliminationOrdering = lexBFS

-- Lexicographical BFS.
-- This implemention is suboptimal (O(|V|^2)), but simple.
lexBFS :: InterferenceGraph -> [String]
lexBFS (IG g) = go [] [Map.keys g]
  where go _   [[]] = []  -- Possible when the graph is empty (no interferences).
        go acc sets
          | all isSingleton sets = concat sets ++ acc
          | otherwise            = goOne acc sets
        goOne acc ([v]:sets)    = go (v:acc) $ sets      `split` v
        goOne acc ((v:vs):sets) = go (v:acc) $ (vs:sets) `split` v
        goOne _ ([]:_)          = error "internal error. lexBFS: empty set"
        goOne acc []            = acc
        split [] _ = []
        split (set:sets) v =
            let (neighbours, nonneighbours) = partition (isNeighbour v) set
            in if any null [neighbours, nonneighbours]
                 then set:(sets `split` v)
                 else nonneighbours:neighbours:(sets `split` v)
        isNeighbour v u = u `Set.member` iNodeOut (g Map.! v)

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
