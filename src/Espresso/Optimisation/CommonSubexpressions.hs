-- Implementation of Global Common Subexpression Elimination for Espresso.
-- Since this step has proven to be computationally costly, hashmaps are utilised.
module Espresso.Optimisation.CommonSubexpressions where

import           Control.Monad.State
import qualified Data.HashMap.Strict           as Map
import           Data.Hashable
import           Data.Int
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

data Subexpression = SOp (Val ()) (Op ()) (Val ())
                   | SStr String
                   | SUnOp (Val ()) (UnOp ())
                   | SPhi [PhiVariant ()] deriving Eq

instance Hashable Subexpression where
    hashWithSalt salt val = case val of
        SOp v1 op v2          -> hashWithSalt salt (0 :: Int8, v1, op, v2)
        SStr s                -> hashWithSalt salt (1 :: Int8, s, False, False)
        SUnOp v op            -> hashWithSalt salt (2 :: Int8, v, op, False)
        SPhi vs               -> hashWithSalt salt (3 :: Int8, map (\(PhiVar _ _ v) -> v) vs, False, False)

-- Run GCSE.
globalCommonSubexpressionElimination :: SSA Liveness -> SSA ()
globalCommonSubexpressionElimination (SSA g) = SSA $ linearMap (\n -> n {nodeCode = eliminate $ nodeCode n}) g
data GCSEState = St {
    subexprs :: Map.HashMap Subexpression ValIdent,
    dict     :: Map.HashMap ValIdent ValIdent
}

-- Since the input is in SSA the operation is a simple static replacement of identical right-hand-sides.
-- The induced copies will be eliminated later.
eliminate :: [Instr Liveness] -> [Instr ()]
eliminate instrs = evalState (mapM go instrs >>= mapM replaceInInstr) (St Map.empty Map.empty)
    where go instr = do
            let live = liveOut $ single instr
            instr' <- replaceInInstr (() <$ instr)
            case split instr' of
                Just (vi, subexpr) -> do
                    mbvi <- gets (Map.lookup subexpr . subexprs)
                    case mbvi of
                        Just vi' -> do
                            let (_, t) = live Map.! toStr vi
                            modify (\st -> st {dict = Map.insert vi vi' (dict st)})
                            return $ ISet () vi (VVal () t vi')
                        Nothing -> do
                            modify (\st -> st {subexprs = Map.insert subexpr vi (subexprs st)})
                            return instr'
                Nothing -> return instr'


replaceInInstr :: Instr () -> State GCSEState (Instr ())
replaceInInstr instr = case instr of
    IRet _ v -> do
        x <- replaceInVal v
        return $ IRet () x
    IOp _ vi v1 op v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ IOp () vi x1 op x2
    ISet _ vi v -> do
        x <- replaceInVal v
        return $ ISet () vi x
    IUnOp _ vi op v -> do
        x <- replaceInVal v
        return $ IUnOp () vi op x
    IVCall _ call -> do
        call' <- replaceInCall call
        return $ IVCall () call'
    ICall _ vi call -> do
        call' <- replaceInCall call
        return $ ICall () vi call'
    INewArr _ vi t v -> do
        x <- replaceInVal v
        return $ INewArr () vi t x
    ICondJmp _ v l1 l2 -> do
        x <- replaceInVal v
        return $ ICondJmp () x l1 l2
    ILoad _ vi ptr -> do
        ptr' <- replaceInPtr ptr
        return $ ILoad () vi ptr'
    IStore _ v ptr -> do
        x <- replaceInVal v
        ptr' <- replaceInPtr ptr
        return $ IStore () x ptr'
    IPhi _ vi phiVars -> do
        phiVars' <- mapM replaceInPhiVar phiVars
        return $ IPhi () vi phiVars'
    _ -> return instr

replaceInCall :: Call () -> State GCSEState (Call ())
replaceInCall call = case call of
    Call _ t qi vs -> do
        xs <- mapM replaceInVal vs
        return $ Call () t qi xs
    CallVirt _ t qi vs -> do
        xs <- mapM replaceInVal vs
        return $ CallVirt () t qi xs

replaceInPhiVar :: PhiVariant () -> State GCSEState (PhiVariant ())
replaceInPhiVar (PhiVar _ l val) = PhiVar () l <$> replaceInVal val

replaceInPtr :: Ptr () -> State GCSEState (Ptr ())
replaceInPtr ptr = case ptr of
    PArrLen _ v -> do
        x <- replaceInVal v
        return $ PArrLen () x
    PElem _ t v1 v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ PElem () t x1 x2
    PFld _ t v qi -> do
        x <- replaceInVal v
        return $ PFld () t x qi
    PLocal {} -> return ptr
    PParam {} -> return ptr

replaceInVal :: Val () -> State GCSEState (Val ())
replaceInVal val = case val of
    VVal _ t vi -> do
        mbvi <- gets (Map.lookup vi . dict)
        return $ case mbvi of
            Just vi' -> VVal () t vi'
            Nothing  -> VVal () t vi
    _ -> return val

-- If the instruction is a valid one to propagate, split it into
-- the left-hand-side (identifier) and the right-hand-side (subexpression).
split :: Instr () -> Maybe (ValIdent, Subexpression)
split instr = case instr of
    IOp _ vi v1 op v2 -> Just (vi, SOp v1 op v2)
    INewStr _ vi s    -> Just (vi, SStr s)
    IUnOp _ vi op v   -> Just (vi, SUnOp v op)
    IPhi _ vi vs      -> Just (vi, SPhi vs)
    -- The following are not valid to propagate.
    ILabel {}         -> Nothing -- Not an assignment.
    ILabelAnn {}      -> Nothing -- Not an assignment.
    IVRet {}          -> Nothing -- Not an assignment.
    IRet {}           -> Nothing -- Not an assignment.
    ISet {}           -> Nothing -- Handled by propagation.
    ISwap {}          -> Nothing -- Nontrivial memory operation.
    IVCall {}         -> Nothing -- Not an assignment.
    ICall {}          -> Nothing -- Can have sideffects
    INew {}           -> Nothing -- Has sideffects.
    INewArr {}        -> Nothing -- Has sideffects.
    IJmp {}           -> Nothing -- Not an assignment.
    ICondJmp {}       -> Nothing -- Not an assignment.
    ILoad {}          -> Nothing -- Nontrivial memory operation.
    IStore {}         -> Nothing -- Nontrivial memory operation.
    IEndPhi {}        -> Nothing -- Not an assignment.

fuse :: ValIdent -> Subexpression -> Instr ()
fuse vi subexpr = case subexpr of
    SOp v1 op v2 -> IOp () vi v1 op v2
    SStr s       -> INewStr () vi s
    SUnOp v op   -> IUnOp () vi op v
    SPhi vs      -> IPhi () vi vs
