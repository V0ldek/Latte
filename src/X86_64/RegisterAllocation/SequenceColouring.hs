module X86_64.RegisterAllocation.SequenceColouring (sequenceColouring) where

import           Data.List
import qualified Data.Map                                    as Map
import           Data.Maybe
import qualified Data.Set                                    as Set
import           X86_64.RegisterAllocation.InterferenceGraph
import           X86_64.Registers

sequenceColouring :: [String] -> InterferenceGraph -> InterferenceGraph
sequenceColouring nodes g = foldl' go g nodes

go :: InterferenceGraph -> String -> InterferenceGraph
go (IG g) n =
    let node = g Map.! n
        neighbourColours = Set.map (iNodeColour . (g Map.!)) (iNodeOut node)
        usedColours = Set.map fromJust $ Set.filter isJust neighbourColours
        colour = find (not . (`Set.member` usedColours)) colours
    in if isJust $ iNodeColour node
         then IG g
         else IG $ Map.insert n (node {iNodeColour = colour}) g

colours :: [Colour]
colours =
    let regs = [rax, rdx, rbx, rcx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]
    in  map ColReg regs ++ map ColStack [0..]

