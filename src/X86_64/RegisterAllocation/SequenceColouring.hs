module X86_64.RegisterAllocation.SequenceColouring (sequenceColouring) where

import           Control.Monad
import           Data.List
import qualified Data.Map                                    as Map
import           Data.Maybe
import qualified Data.Set                                    as Set
import           Utilities
import           X86_64.RegisterAllocation.InterferenceGraph
import           X86_64.Registers

-- Attempt to colour the graph with registers.
-- If successful, return Right colouring.
-- Otherwise, return Left nodes that were uncoloured and their neighbours as possible candidates
-- for spilling.
sequenceColouring :: [String] -> InterferenceGraph -> Either [InterferenceNode] InterferenceGraph
sequenceColouring nodes g =
    let tryColoured = foldl' go g nodes
    in case Map.elems $ Map.filter (isNothing . iNodeColour) (ig tryColoured) of
        [] -> Right tryColoured
        xs -> Left $ dedupBy iNodeLabel $ concatMap (\x -> x:neighbours g x) xs

go :: InterferenceGraph -> String -> InterferenceGraph
go (IG g) n =
    case Map.lookup n g of
        Just node ->
            let colour = colourForNode (IG g) node
            in if isJust $ iNodeColour node
                then IG g
                else IG $ Map.insert n (node {iNodeColour = colour}) g
        Nothing -> error $ "internal error. sequenceColouring: node not found " ++ n

-- Find a colour for the node that does not collide with neighbours.
-- Greedily prefer the preferred type of register.
colourForNode :: InterferenceGraph -> InterferenceNode -> Maybe Reg
colourForNode (IG g) node =
    let neighbourColours = Set.map (iNodeColour . (g Map.!)) (iNodeOut node)
        usedColours = Set.map fromJust $ Set.filter isJust neighbourColours
        freeColours = filter (not . (`Set.member` usedColours)) allRegs
        prefColours = filter ((== iNodeRegPref node) . regType) freeColours
        prefColour = find (const True) prefColours
        freeColour = find (const True) freeColours
    in msum [prefColour, freeColour]

neighbours :: InterferenceGraph -> InterferenceNode -> [InterferenceNode]
neighbours (IG g) node = map (g Map.!) (Set.elems (iNodeOut node))
