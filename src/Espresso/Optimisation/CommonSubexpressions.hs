module Espresso.Optimisation.CommonSubexpressions where

import           Control.Monad.State
import qualified Data.HashMap.Strict           as Map
import           Data.Hashable
import           Data.Int
import qualified Data.Map                      as OrdMap
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

data Subexpression = SOp (Val ()) (Op ()) (Val ())
                   | SStr String
                   | SUnOp (Val ()) (UnOp ())
                   | SFld (Val ()) (QIdent ())
                   | SArr (Val ()) (Val ())
                   | SArrLen (Val ())
                   | SPhi [PhiVariant ()] deriving Eq

instance Hashable Subexpression where
    hashWithSalt salt val = case val of
        SOp v1 op v2          -> hashWithSalt salt (0 :: Int8, v1, op, v2)
        SStr s                -> hashWithSalt salt (1 :: Int8, s, False, False)
        SUnOp v op            -> hashWithSalt salt (2 :: Int8, v, op, False)
        SFld v (QIdent _ _ i) -> hashWithSalt salt (3 :: Int8, v, i, False)
        SArr v1 v2            -> hashWithSalt salt (4 :: Int8, v1, v2, False)
        SArrLen v             -> hashWithSalt salt (5 :: Int8, v, False, False)
        SPhi vs               -> hashWithSalt salt (6 :: Int8, map (\(PhiVar _ _ v) -> v) vs, False, False)

globalCommonSubexpressionElimination :: SSA Liveness -> SSA ()
globalCommonSubexpressionElimination (SSA g) = SSA $ linearMap (\n -> n {nodeCode = eliminate $ nodeCode n}) g

data GCSEState = St {
    subexprs :: Map.HashMap Subexpression ValIdent,
    dict     :: Map.HashMap ValIdent ValIdent
}

eliminate :: [Instr Liveness] -> [Instr ()]
eliminate instrs = evalState (mapM go instrs) (St Map.empty Map.empty)
    where go instr = do
            let live = liveOut $ single instr
            instr' <- replaceInInstr (() <$ instr)
            case split instr' of
                Just (vi, subexpr) -> do
                    mbvi <- gets (Map.lookup subexpr . subexprs)
                    case mbvi of
                        Just vi' -> do
                            let (_, t) = live OrdMap.! toStr vi
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
    ILoad _ vi v -> do
        x <- replaceInVal v
        return $ ILoad () vi x
    IStore _ v1 v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ IStore () x1 x2
    IFld _ vi v qi -> do
        x <- replaceInVal v
        return $ IFld () vi x qi
    IArr _ vi v1 v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ IArr () vi x1 x2
    IArrLen _ vi v -> do
        x <- replaceInVal v
        return $ IArrLen () vi x
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

replaceInVal :: Val () -> State GCSEState (Val ())
replaceInVal val = case val of
    VVal _ t vi -> do
        mbvi <- gets (Map.lookup vi . dict)
        return $ case mbvi of
            Just vi' -> VVal () t vi'
            Nothing  -> VVal () t vi
    _ -> return val

split :: Instr () -> Maybe (ValIdent, Subexpression)
split instr = case instr of
    IOp _ vi v1 op v2 -> Just $ (vi, SOp v1 op v2)
    IStr _ vi s       -> Just $ (vi, SStr s)
    IUnOp _ vi op v   -> Just $ (vi, SUnOp v op)
    IFld _ vi v qi    -> Just $ (vi, SFld v qi)
    IArr _ vi v1 v2   -> Just $ (vi, SArr v1 v2)
    IArrLen _ vi v    -> Just $ (vi, SArrLen v)
    IPhi _ vi vs      -> Just $ (vi, SPhi vs)
    _                 -> Nothing

fuse :: ValIdent -> Subexpression -> Instr ()
fuse vi subexpr = case subexpr of
    SOp v1 op v2 -> IOp () vi v1 op v2
    SStr s       -> IStr () vi s
    SUnOp v op   -> IUnOp () vi op v
    SFld v qi    -> IFld () vi v qi
    SArr v1 v2   -> IArr () vi v1 v2
    SArrLen v    -> IArrLen () vi v
    SPhi vs      -> IPhi () vi vs
