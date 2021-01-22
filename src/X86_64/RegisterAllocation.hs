module X86_64.RegisterAllocation where

import           Data.Bifunctor
import           Data.List
import qualified Data.Map                                             as Map
import           Data.Maybe
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Optimisation.DeadCode
import           Espresso.Syntax.Abs
import           Utilities
import           X86_64.RegisterAllocation.InterferenceGraph
import           X86_64.RegisterAllocation.PerfectEliminationOrdering
import           X86_64.RegisterAllocation.SequenceColouring
import           X86_64.RegisterAllocation.Spilling
import           X86_64.Registers

data RegisterAllocation = RegAlloc {
    regAlloc  :: Map.Map ValIdent Reg,
    numLocals :: Int
}

-- Colour the interference graph, spilling variables if necessary.
getColouredInterferenceGraph :: SSA Liveness -> (InterferenceGraph, SSA Liveness)
getColouredInterferenceGraph (SSA g_) = go g_ 0
    where
        go g spilledLocals =
            let interference = buildInterferenceGraph g
                order = perfectEliminationOrdering interference
            in case sequenceColouring order interference of
                Right coloured -> (coloured, removeDeadCodeSSA $ SSA g)
                Left ns ->
                    let spills = map (spill g spilledLocals) ns
                        best   = head $ sortOn estCost spills
                        newCFG = analyseLiveness (spilledCFG best)
                    in go newCFG (spilledLocals + 1)

getRegisterAllocation :: CFG a -> InterferenceGraph -> RegisterAllocation
getRegisterAllocation (CFG cfg_) ig_ =
    let ptrLocals = concatMap (mapMaybe findPtrLocals . nodeCode) $ Map.elems cfg_
        locCnt = length $ dedup ptrLocals
        alloc = Map.fromList $ map (first ValIdent) $ Map.toList (getColouring ig_)
    in  RegAlloc alloc locCnt

usedRegs :: RegisterAllocation -> [Reg]
usedRegs (RegAlloc r _) = dedup $ Map.elems r

findPtrLocals :: Instr a -> Maybe Integer
findPtrLocals instr = case instr of
    ILoad _ _ (PLocal _ _ n)  -> Just n
    IStore _ _ (PLocal _ _ n) -> Just n
    _                         -> Nothing
