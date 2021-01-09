-- Annotations of variable liveness for CFGs.
module Espresso.ControlFlow.Liveness where

import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG (CFG (..), Node (..))
import           Espresso.Syntax.Abs
import           Utilities

type VarSet = Set.Set String
-- A map between variables and the shortest distance
-- (in number of instructions) to its next use
type NextUse = Map.Map String Int

-- Liveness annotations for a single instruction.
data Liveness = Liveness {
    -- Variables alive at the start of the instruction and their next uses.
    liveIn   :: NextUse,
    -- Variables alive at the end of the instruction and their next uses.
    liveOut  :: NextUse,
    -- Variables used in this instruction.
    liveUse  :: VarSet,
    -- Variables killed in this instruction.
    liveKill :: VarSet
} deriving Eq

-- Neutral element with no variables alive or used.
emptyLiveness :: Liveness
emptyLiveness = Liveness Map.empty Map.empty Set.empty Set.empty

-- Annotate a CFG with liveness data for every instruction in every node.
-- This is done by iterating the liveness data until reaching a fixpoint,
-- in each step propagating the liveness data locally within a basic block.
analyseLiveness :: CFG a -> CFG Liveness
analyseLiveness g = let start = initialLiveness g
                    in  fixpoint iterateLiveness start

-- Starting point for the algorithm containing use and kill data for each instruction.
initialLiveness :: CFG a -> CFG Liveness
initialLiveness (CFG g) = CFG (Map.map nodeInitLive g)
    where nodeInitLive node = node { nodeCode = map instrInitLive (nodeCode node) }
          instrInitLive instr =
              let (use, kill) = case instr of
                    IRet _ v         -> (valSet v, Set.empty)
                    IOp _ vi v1 _ v2 -> (valsSet [v1, v2], valISet vi)
                    ISet _ vi v      -> (valSet v, valISet vi)
                    IUnOp _ vi _ v   -> (valSet v, valISet vi)
                    IVCall _ call    -> (callUseSet call, Set.empty)
                    ICall _ vi call  -> (callUseSet call, valISet vi)
                    ICondJmp _ v _ _ -> (valSet v, Set.empty)
                    ILoad _ vi v     -> (valSet v, valISet vi)
                    IStore _ v1 v2   -> (valsSet [v1, v2], Set.empty)
                    IFld _ vi v _    -> (valSet v, valISet vi)
                    IArr _ vi v1 v2  -> (valsSet [v1, v2], valISet vi)
                    IPhi _ vi phis   -> (phiUseSet phis, valISet vi)
                    _                -> (Set.empty, Set.empty)
              in emptyLiveness { liveUse = use, liveKill = kill } <$ instr

-- Propagate current liveness data locally within basic blocks
-- and then propagate once through edges in the graph.
iterateLiveness :: CFG Liveness -> CFG Liveness
iterateLiveness (CFG g) = globalLiveness $ CFG $ Map.map localLiveness g

-- For each edge (v, u) in the graph mark the variables alive at the beginning of u
-- as alive at the end of v.
globalLiveness :: CFG Liveness -> CFG Liveness
globalLiveness (CFG g) = CFG $ Map.map go g
    where
        go node =
            let -- Look at each outgoing edge and get the next uses of all live variables,
                -- taking the minimum if a variable is used in more than one block.
                out = foldr (Map.unionWith min . liveIn . nodeLiveness . (g Map.!)) Map.empty (Set.elems $ nodeOut node)
                (lastInstr, instrs) = splitLast $ nodeCode node
                -- Put the data from the target nodes in the last instruction to be propagated
                -- during localLiveness step.
                lastInstr' = (\l -> l {liveOut = Map.map (+1) out}) <$> lastInstr
            in  node {nodeCode = instrs ++ [lastInstr']}

-- Propagate variable usage down-to-top through a node.
localLiveness :: Node Liveness -> Node Liveness
localLiveness node = node {nodeCode = go (nodeCode node)}
    where
        go :: [Instr Liveness] -> [Instr Liveness]
        go []             = []
        go (instr:instrs) =
            let xs = go instrs
                instr' = (\live ->
                    let -- If this is the last instruction the liveOut
                        -- set will be unchanged in this step.
                        out = case xs of
                            []  -> liveOut live
                            x:_ -> liveIn $ single x
                        -- in = (out - kill) \cup use
                        fromOut = (Map.map (+1) out `Map.withoutKeys` liveKill live)
                        fromThis = Map.fromSet (const 0) (liveUse live)
                        in_ = fromThis `Map.union` fromOut
                    in  live {liveOut = out, liveIn = in_})
                    <$> instr
            in  instr':xs

valISet :: ValIdent -> VarSet
valISet (ValIdent s) = Set.singleton s

valSet :: Val a -> VarSet
valSet v = case v of
    VVal _ _ vi -> valISet vi
    _           -> Set.empty

valsSet :: [Val a] -> VarSet
valsSet = foldr (Set.union . valSet) Set.empty

callUseSet :: Call a -> VarSet
callUseSet call = case call of
    Call _ _ _ vs     -> valsSet vs
    CallVirt _ _ _ vs -> valsSet vs

phiUseSet :: [PhiVariant a] -> VarSet
phiUseSet phis = valsSet $ map (\(PhiVar _ _ v) -> v) phis

nodeLiveness :: Node Liveness -> Liveness
nodeLiveness node = single $ head $ nodeCode node

instance Show Liveness where
    show l = "in = " ++ showMap (liveIn l) ++
             ", out = " ++ showMap (liveOut l) ++
             ", use = " ++ showSet (liveUse l) ++
             ", kill = " ++ showSet (liveKill l)
        where showSet = show . Set.elems
              showMap = show . Map.toList
