module X86_64.RegisterAllocation.Spilling where

import           Control.Monad.State
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.HashSet                                as HashSet
import qualified Data.Map                                    as Map
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities
import           X86_64.RegisterAllocation.InterferenceGraph

data Spill = Spill {
    spilledCFG :: CFG (),
    spilledVar :: String,
    estCost    :: Float
}

newtype SpillState = St {
    idx :: Integer
}

freshIdx :: State SpillState Integer
freshIdx = do
    i <- gets idx
    modify (\st -> st{idx = i + 1})
    return i

spill :: CFG Liveness -> Integer -> InterferenceNode -> Spill
spill (CFG g) locN iNode = evalState go (St 0)
    where go = do
            spills <- mapM (spillInBlock (ValIdent $ iNodeLabel iNode) locN) (Map.elems g)
            let combCost = foldr (\n x -> x + costInNode (iNodeLabel iNode) n locN) 0 spills
                combCost' = if combCost == 0 then read "Infinity" else combCost
                g' = Map.fromList $ map (\n -> (nodeLabel n, n)) spills
            return $ Spill (CFG g') (iNodeLabel iNode) (combCost' / fromIntegral (length (iNodeOut iNode)))

spillInBlock :: ValIdent -> Integer -> Node Liveness -> State SpillState (Node ())
spillInBlock vi locN node = do
    instrs' <- addLoad vi locN (addStore vi locN (nodeCode node))
    return node {nodeCode = instrs'}

addStore :: ValIdent -> Integer -> [Instr Liveness] -> [Instr Liveness]
addStore _ _ [] = []
addStore vi locN (instr:instrs) =
    let live = single instr
    in case HashMap.lookup (toStr vi) (liveOut live) of
        Just (_, t) | HashSet.member (toStr vi) (liveKill live) ->
            let t' = emptyLiveness <$ t
            in  instr:IStore emptyLiveness (VVal emptyLiveness t' vi) (PLocal emptyLiveness t' locN):addStore vi locN instrs
        _ -> instr:addStore vi locN instrs

addLoad :: ValIdent -> Integer -> [Instr Liveness] -> State SpillState [Instr ()]
addLoad _ _ [] = return []
addLoad vi locN (instr:instrs) =
    let instr' = () <$ instr
        live = single instr
    in case HashMap.lookup (toStr vi) (liveUse live) of
         Just t -> do
             i <- freshIdx
             let vi' = suffix vi locN i
             rest <- addLoad vi locN instrs
             return $ ILoad () vi' (PLocal () t locN):
                      rename vi vi' instr':rest
         Nothing -> (instr':) <$> addLoad vi locN instrs

costInNode :: String -> Node a -> Integer -> Float
costInNode vi node locN = foldr (\i x -> instrCost vi i locN + x) 0 (nodeCode node)

instrCost :: String -> Instr a -> Integer -> Float
instrCost _ (ILoad _ _ (PLocal _ _ loc)) locN | loc == locN = 1
instrCost vi (ILoad _ vi' PLocal {}) _ | vi == toStr vi'    = read "Infinity"
instrCost _ _ _ = 0

suffix :: ValIdent -> Integer -> Integer -> ValIdent
suffix (ValIdent vi) n i = ValIdent $ vi ++ "~loc_" ++ show n ++ "_" ++ show i

rename :: ValIdent -> ValIdent -> Instr () -> Instr ()
rename vif vit instr = case instr of
    IRet _ v           -> IRet () (f v)
    IOp _ vi v1 op v2  -> IOp () vi (f v1) op (f v2)
    ISet _ vi v        -> ISet () vi (f v)
    IUnOp _ vi op v    -> IUnOp () vi op (f v)
    IVCall _ call      -> IVCall () (fc call)
    ICall _ vi call    -> ICall () vi (fc call)
    INewArr _ vi t v   -> INewArr () vi t (f v)
    ICondJmp _ v l1 l2 -> ICondJmp () (f v) l1 l2
    IPhi _ vi phiVars  -> IPhi () vi (map fp phiVars)
    ISwap {}           -> error "swap should not occur before phi removal"
    _                  -> () <$ instr
    where
        f (VVal _ t vi) | vi == vif = VVal () t vit
        f val = val
        fc (Call _ t qi vs)     = Call () t qi (map f vs)
        fc (CallVirt _ t qi vs) = CallVirt () t qi (map f vs)
        fp (PhiVar _ l v) = PhiVar () l (f v)
