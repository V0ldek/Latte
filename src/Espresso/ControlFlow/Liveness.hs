module Espresso.ControlFlow.Liveness where

import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG (CFG (..), Node (..))
import           Espresso.Syntax.Abs
import           Utilities

type VarSet = Set.Set String
type NextUse = Map.Map String Int
data Liveness = Liveness {
    liveIn   :: NextUse,
    liveOut  :: NextUse,
    liveUse  :: VarSet,
    liveKill :: VarSet
} deriving Eq

emptyLiveness :: Liveness
emptyLiveness = Liveness Map.empty Map.empty Set.empty Set.empty

analyseLiveness :: CFG a -> CFG Liveness
analyseLiveness g = let start = initialLiveness g
                    in  fixpoint iterateLiveness start

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
                    IStore _ v1 v2   -> (Set.empty, valsSet [v1, v2])
                    IFld _ vi v _    -> (valSet v, valISet vi)
                    IArr _ vi v1 v2  -> (valsSet [v1, v2], valISet vi)
                    IPhi _ vi phis   -> (phiUseSet phis, valISet vi)
                    _                -> (Set.empty, Set.empty)
              in emptyLiveness { liveUse = use, liveKill = kill } <$ instr

iterateLiveness :: CFG Liveness -> CFG Liveness
iterateLiveness (CFG g) = globalLiveness $ CFG $ Map.map localLiveness g

globalLiveness :: CFG Liveness -> CFG Liveness
globalLiveness (CFG g) = CFG $ Map.map go g
    where
        go node = let out = foldr (Map.unionWith min . liveIn . nodeLiveness . (g Map.!)) Map.empty (Set.elems $ nodeOut node)
                      (lastInstr, instrs) = splitLast $ nodeCode node
                      lastInstr' = (\l -> l {liveOut = Map.map (+1) out}) <$> lastInstr
                  in  node {nodeCode = instrs ++ [lastInstr']}

localLiveness :: Node Liveness -> Node Liveness
localLiveness node = node {nodeCode = go (nodeCode node)}
    where
        go :: [Instr Liveness] -> [Instr Liveness]
        go []             = []
        go (instr:instrs) =
            let xs = go instrs
                instr' = (\live ->
                    let out = case xs of
                            []  -> liveOut live
                            x:_ -> liveIn $ single x
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
    show l = "in, nextUse = " ++ showMap (liveIn l) ++
             ", out, nextUse = " ++ showMap (liveOut l) ++
             ", use = " ++ showSet (liveUse l) ++
             ", kill = " ++ showSet (liveKill l)
        where showSet = show . Set.elems
              showMap = show . Map.toList
