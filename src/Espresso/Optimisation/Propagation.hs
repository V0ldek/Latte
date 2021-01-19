module Espresso.Optimisation.Propagation where

import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.Map                 as Map
import           Data.Maybe
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Phi
import           Espresso.ControlFlow.SSA
import           Espresso.Syntax.Abs
import           Utilities

data ValKind = ValSimple (Val ())
             | ValString String
             | ValComplex
    deriving Eq

newtype PropagationState = St {
    values :: Map.Map ValIdent ValKind
} deriving Eq

propagateCopiesAndConsts :: SSA () -> Method () -> SSA ()
propagateCopiesAndConsts (SSA g) (Mthd _ t qi ps _) =
    let instrs = linearise g
        psKinds = map (\(Param _ _ vi) -> (vi, ValComplex)) ps
        f (is, s) = first concat $ runState (mapM propagate is) s
        (instrs', _) = fixpointBy (Map.size . values . snd) f (instrs, St $ Map.fromList psKinds)
    in  SSA $ cfg (Mthd () t qi ps instrs')

propagate :: Instr () -> State PropagationState [Instr ()]
propagate instr = case instr of
    IRet _ val -> do
        x <- tryPropagate val
        return [IRet () x]
    IOp _ vi val1 op val2 -> do
        x1 <- tryPropagate val1
        x2 <- tryPropagate val2
        mbstr1 <- tryString x1
        mbstr2 <- tryString x2
        case (mbstr1, mbstr2) of
            (Just s1, Just s2) -> do
                setString vi (s1 ++ s2)
                return [IStr () vi (s1 ++ s2)]
            _ -> do
                let simplified = trySimplifyBinOp x1 op x2
                case simplified of
                    Just val -> do
                        setSimple vi val
                        return []
                    Nothing  -> do
                        setComplex vi
                        return [IOp () vi x1 op x2]
    ISet _ vi val -> do
        x <- tryPropagate val
        setSimple vi x
        return []
    IStr _ vi str -> do
        setString vi str
        return [IStr () vi str]
    IUnOp _ vi unOp val -> do
        x <- tryPropagate val
        let simplified = trySimplifyUnOp x unOp
        case simplified of
            Just val' -> do
                setSimple vi val'
                return []
            Nothing -> do
                setComplex vi
                return [IUnOp () vi unOp x]
    IVCall _ call -> do
        call' <- propagateInCall call
        return [IVCall () call']
    ICall _ vi call -> do
        call' <- propagateInCall call
        setComplex vi
        return [ICall () vi call']
    INew _ vi t -> do
        setComplex vi
        return [INew () vi t]
    INewArr _ vi t val -> do
        x <- tryPropagate val
        setComplex vi
        return [INewArr () vi t x]
    ICondJmp _ val l1 l2 -> do
        x <- tryPropagate val
        return $ case x of
            VTrue _  -> [IJmp () l1]
            VFalse _ -> [IJmp () l2]
            _        -> [ICondJmp () x l1 l2]
    ILoad _ vi val -> do
        x <- tryPropagate val
        setComplex vi
        return [ILoad () vi x]
    IStore _ val1 val2 -> do
        x1 <- tryPropagate val1
        x2 <- tryPropagate val2
        return [IStore () x1 x2]
    IFld _ vi val qi -> do
        x <- tryPropagate val
        setComplex vi
        return [IFld () vi x qi]
    IArr _ vi val1 val2 -> do
        x1 <- tryPropagate val1
        x2 <- tryPropagate val2
        setComplex vi
        return [IArr () vi x1 x2]
    IArrLen _ vi val -> do
        x <- tryPropagate val
        setComplex vi
        return [IArrLen () vi x]
    IPhi _ vi phiVar -> do
        setComplex vi
        phiVar' <- mapM propagateInPhiVar phiVar
        let mbphi = unfoldTrivialPhi (IPhi () vi phiVar')
        return $ maybeToList mbphi
    _ -> return [instr]

propagateInCall :: Call () -> State PropagationState (Call ())
propagateInCall call = case call of
    Call _ t qi vals -> do
        xs <- mapM tryPropagate vals
        return $ Call () t qi xs
    CallVirt _ t qi vals -> do
        xs <- mapM tryPropagate vals
        return $ CallVirt () t qi xs

propagateInPhiVar :: PhiVariant () -> State PropagationState (PhiVariant ())
propagateInPhiVar (PhiVar _ l val) = do
    x <- tryPropagate val
    return $ PhiVar () l x

setComplex :: ValIdent -> State PropagationState ()
setComplex vi = modify (St . Map.insert vi ValComplex . values)

setString :: ValIdent -> String -> State PropagationState ()
setString vi str = modify (St . Map.insert vi (ValString str) . values)

setSimple :: ValIdent -> Val () -> State PropagationState ()
setSimple vi val = do
    x <- tryPropagate val
    let vk = ValSimple x
    modify (St . Map.insert vi vk . values)

tryPropagate :: Val () -> State PropagationState (Val ())
tryPropagate val = case val of
    (VVal _ _ vi) -> do
        mbvk <- gets (Map.lookup vi . values)
        case mbvk of
            Just (ValSimple val') -> return val'
            Just ValComplex       -> return val
            Just (ValString _)    -> return val
            Nothing               -> return val
    _ -> return val

tryString :: Val () -> State PropagationState (Maybe String)
tryString val = case val of
    (VVal _ _ vi) -> do
        mbvk <- gets (Map.lookup vi . values)
        case mbvk of
            Just (ValString s) -> return $ Just s
            _                  -> return Nothing
    _             -> return Nothing

trySimplifyBinOp :: Val () -> Op () -> Val () -> Maybe (Val ())
trySimplifyBinOp v1 op v2 = case op of
    OpAdd _ -> simplifyIntBinOp (+)
    OpSub _ -> simplifyIntBinOp (-)
    OpMul _ -> simplifyIntBinOp (*)
    OpDiv _ -> simplifyIntBinOp div
    OpMod _ -> simplifyIntBinOp mod
    OpLTH _ -> simplifyRelBinOp (<)
    OpLE _  -> simplifyRelBinOp (<=)
    OpGTH _ -> simplifyRelBinOp (>)
    OpGE _  -> simplifyRelBinOp (>=)
    OpEQU _ -> simplifyEqBinOp id
    OpNE _  -> simplifyEqBinOp not
    where
        simplifyIntBinOp intOp = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ VInt () (n1 `intOp` n2)
            (VInt _ n1, VNegInt _ n2)    -> Just $ VInt () (n1 `intOp` (-n2))
            (VNegInt _ n1, VInt _ n2)    -> Just $ VInt () ((-n1) `intOp` n2)
            (VNegInt _ n1, VNegInt _ n2) -> Just $ VInt () ((-n1) `intOp` (-n2))
            _                            -> Nothing
        simplifyRelBinOp relOp = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ boolToVal (n1 `relOp` n2)
            (VInt _ n1, VNegInt _ n2)    -> Just $ boolToVal (n1 `relOp` (-n2))
            (VNegInt _ n1, VInt _ n2)    -> Just $ boolToVal ((-n1) `relOp` n2)
            (VNegInt _ n1, VNegInt _ n2) -> Just $ boolToVal ((-n1) `relOp` (-n2))
            (VFalse _, VFalse _)         -> Just $ boolToVal (0 `relOp` 0)
            (VFalse _, VTrue _)          -> Just $ boolToVal (0 `relOp` 1)
            (VTrue _, VFalse _)          -> Just $ boolToVal (1 `relOp` 0)
            (VTrue _, VTrue _)           -> Just $ boolToVal (1 `relOp` 1)
            (VVal _ _ vi1, VVal _ _ vi2)
                | vi1 == vi2             -> Just $ boolToVal (1 `relOp` 1)
            _                            -> Nothing
        simplifyEqBinOp modif = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ boolToVal $ modif $ n1 == n2
            (VInt _ n1, VNegInt _ n2)    -> Just $ boolToVal $ modif $ n1 == (-n2)
            (VNegInt _ n1, VInt _ n2)    -> Just $ boolToVal $ modif $ (-n1) == n2
            (VNegInt _ n1, VNegInt _ n2) -> Just $ boolToVal $ modif $ (-n1) == (-n2)
            (VFalse _, VFalse _)         -> Just $ boolToVal $ modif True
            (VTrue _, VTrue _)           -> Just $ boolToVal $ modif True
            (VTrue _, VFalse _)          -> Just $ boolToVal $ modif False
            (VFalse _, VTrue _)          -> Just $ boolToVal $ modif False
            (VNull _ _, VNull _ _)       -> Just $ boolToVal $ modif True
            (VVal _ _ vi1, VVal _ _ vi2)
                | vi1 == vi2             -> Just $ boolToVal $ modif True
            _                            -> Nothing

trySimplifyUnOp :: Val () -> UnOp () -> Maybe (Val ())
trySimplifyUnOp val unOp = case unOp of
    UnOpNeg _ -> case val of
        VInt _ n    -> Just $ VInt () (-n)
        VNegInt _ n -> Just $ VInt () n
        _           -> Nothing
    UnOpNot _ -> case val of
        VTrue _  -> Just $ VFalse ()
        VFalse _ -> Just $ VTrue ()
        _        -> Nothing

boolToVal :: Bool -> Val ()
boolToVal True  = VTrue ()
boolToVal False = VFalse ()
