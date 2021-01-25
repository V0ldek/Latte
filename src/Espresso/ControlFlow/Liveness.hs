{-# LANGUAGE TupleSections #-}
-- Annotations of variable liveness for CFGs.
-- This is done multiple times during code generation, including in a loop
-- in optimisation pipeline, so hashed containers are used to amp the performance.
module Espresso.ControlFlow.Liveness where

import           Data.Bifunctor
import qualified Data.HashMap.Strict      as Map
import qualified Data.HashSet             as Set
import qualified Data.Map                 as OrdMap
import qualified Data.Set                 as OrdSet
import           Espresso.ControlFlow.CFG (CFG (..), Node (..), linearMap)
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

type VarSet = Set.HashSet String
type TypedVarSet = Map.HashMap String (SType ())
-- A map between variables and the shortest distance
-- (in number of instructions) to its next use and the
-- type context of that use
type NextUse = Map.HashMap String (Int, SType ())

-- Liveness annotations for a single instruction.
data Liveness = Liveness {
    -- Variables alive at the start of the instruction and their next uses.
    liveIn   :: NextUse,
    -- Variables alive at the end of the instruction and their next uses.
    liveOut  :: NextUse,
    -- Variables used in this instruction and their types.
    liveUse  :: TypedVarSet,
    -- Variables killed in this instruction.
    liveKill :: VarSet
} deriving Eq

-- Neutral element with no variables alive or used.
emptyLiveness :: Liveness
emptyLiveness = Liveness Map.empty Map.empty Map.empty Set.empty

-- Annotate a CFG with liveness data for every instruction in every node.
-- This is done by iterating the liveness data until reaching a fixpoint,
-- in each step propagating the liveness data locally within a basic block.
analyseLiveness :: CFG a -> CFG Liveness
analyseLiveness g = let start = initialLiveness g
                    in  fixpoint iterateLiveness start

-- Starting point for the algorithm containing use and kill data for each instruction.
initialLiveness :: CFG a -> CFG Liveness
initialLiveness = linearMap nodeInitLive
    where nodeInitLive node = node { nodeCode = map instrInitLive (nodeCode node) }
          instrInitLive instr =
              let (use, kill) = case instr of
                    IRet _ v          -> (valSet v, Set.empty)
                    IOp _ vi v1 _ v2  -> (valsSet [v1, v2], valISet vi)
                    ISet _ vi v       -> (valSet v, valISet vi)
                    ISwap _ t vi1 vi2 -> (Map.fromList [(toStr vi2, () <$ t)], valISet vi1)
                    IUnOp _ vi _ v    -> (valSet v, valISet vi)
                    IVCall _ call     -> (callUseSet call, Set.empty)
                    ICall _ vi call   -> (callUseSet call, valISet vi)
                    INew _ vi _       -> (Map.empty, valISet vi)
                    INewArr _ vi _ v  -> (valSet v, valISet vi)
                    INewStr _ vi _    -> (Map.empty, valISet vi)
                    ICondJmp _ v _ _  -> (valSet v, Set.empty)
                    ILoad _ vi p      -> (ptrValSet p, valISet vi)
                    IStore _ v p      -> (valSet v `Map.union` ptrValSet p, Set.empty)
                    IPhi _ vi phis    -> (phiUseSet phis, valISet vi)
                    _                 -> (Map.empty, Set.empty)
              in emptyLiveness { liveUse = use, liveKill = kill } <$ instr

-- Propagate current liveness data locally within basic blocks
-- and then propagate once through edges in the graph.
iterateLiveness :: CFG Liveness -> CFG Liveness
iterateLiveness = globalLiveness . linearMap localLiveness

-- For each edge (v, u) in the graph mark the variables alive at the beginning of u
-- as alive at the end of v.
globalLiveness :: CFG Liveness -> CFG Liveness
globalLiveness cfg_@(CFG g) = linearMap go cfg_
    where
        go node =
            let -- Look at each outgoing edge and get the next uses of all live variables,
                -- taking the minimum if a variable is used in more than one block.
                out = foldr (Map.unionWith min . liveIn . nodeLiveness . (g OrdMap.!)) Map.empty (OrdSet.elems $ nodeOut node)
                (lastInstr, instrs) = splitLast $ nodeCode node
                -- Put the data from the target nodes in the last instruction to be propagated
                -- during localLiveness step.
                lastInstr' = (\l -> l {liveOut = Map.map (first (+1)) out}) <$> lastInstr
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
                        fromOut = Map.filterWithKey (\k _ -> not $ k `Set.member` liveKill live) $ Map.map (first (+1)) out
                        fromThis = Map.map (0,) (liveUse live)
                        in_ = fromThis `Map.union` fromOut
                    in  live {liveOut = out, liveIn = in_})
                    <$> instr
            in  instr':xs

valISet :: ValIdent -> VarSet
valISet (ValIdent s) = Set.singleton s

valSet :: Val a -> TypedVarSet
valSet v = case v of
    VVal _ t (ValIdent s) -> Map.singleton s (() <$ t)
    _                     -> Map.empty

valsSet :: [Val a] -> TypedVarSet
valsSet = foldr (Map.union . valSet) Map.empty

ptrValSet :: Ptr a -> TypedVarSet
ptrValSet ptr = case ptr of
    PArrLen _ v                -> valSet v
    PElem _ _ v1 v2            -> valsSet [v1, v2]
    PFld _ _ v _               -> valSet v
    PLocal {}                  -> Map.empty
    PParam _ t _ (ValIdent vi) -> Map.singleton vi (() <$ t)

callUseSet :: Call a -> TypedVarSet
callUseSet call = case call of
    Call _ _ _ vs     -> valsSet vs
    CallVirt _ _ _ vs -> valsSet vs

phiUseSet :: [PhiVariant a] -> TypedVarSet
phiUseSet phis = valsSet $ map (\(PhiVar _ _ v) -> v) phis

nodeLiveness :: Node Liveness -> Liveness
nodeLiveness node = single $ head $ nodeCode node

instance Show Liveness where
    show l = "in = " ++ showMap (liveIn l) ++
             ", out = " ++ showMap (liveOut l) ++
             ", use = " ++ showMap (liveUse l) ++
             ", kill = " ++ showSet (liveKill l)
        where showSet = show . Set.toList
              showMap :: (Show a, Show b) => Map.HashMap a b -> String
              showMap = show . Map.toList
