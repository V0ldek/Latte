{-# LANGUAGE FlexibleInstances #-}
module X86_64.CodeGen.GenM (
      fullTrace,
      getClass,
      getLoc,
      getPreservedRegs,
      getValLoc,
      getVarS,
      isLive,
      label,
      newStrConst,
      setStack,
      traceM',
      updateLive,
      updateLocs,
      varSize,
      CompiledMethod(..),
      Env(..),
      GenM,
      Store(..),
      VarKind(..),
      VarState(..)
) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Data.Int
import qualified Data.Map                      as Map
import           Debug.Trace
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Class
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit           as Emit
import           X86_64.CodeGen.Stack
import           X86_64.Loc
import           X86_64.RegisterAllocation
import           X86_64.Registers
import           X86_64.Size

traceEnabled :: Bool
traceEnabled = False

data VarState = VarS {
    varName :: ValIdent,
    varType :: SType (),
    varLoc  :: Loc
}

data VarKind = VarImm
             | VarPtr Int64

data Store = St {
    -- All code generated thus far, in reverse.
    allCode  :: [String],
    -- All code generated for the current basic block, in reverse.
    bbCode   :: [String],
    -- All string constants used thus far.
    consts   :: ConstSet,
    -- The current state of the stack.
    stack    :: Stack,
    -- Descriptions of variables.
    vars     :: Map.Map ValIdent VarState,
    -- Currently live variables and their next usage.
    live     :: Liveness,
    traceIdx :: Integer -- debug
}

data Env = Env {
    -- Generator of labels for the current method.
    labelGen :: LabIdent -> LabIdent,
    -- Location colouring of variables.
    regs     :: RegisterAllocation,
    -- Type metadata.
    classes  :: Map.Map SymIdent CompiledClass
}

data CompiledMethod = CmpMthd {
    -- Label of the method start, the target for calls.
    mthdEntry    :: String,
    mthdPrologue :: [String],
    mthdCode     :: [String],
    mthdEpilogue :: [String]
}

type GenM = StateT Store (Reader Env)

instance Emit.EmitM GenM where
    emit s = modify (\st -> st {bbCode = s:bbCode st})

updateLive :: Liveness -> GenM ()
updateLive l = do
    modify (\st -> st {live = l})
    updateLocs

updateLocs :: GenM ()
updateLocs = do
    l <- gets live
    forM_ (HashMap.toList $ liveIn l `HashMap.union` liveOut l) updateLoc
    where
        updateLoc :: (String, (Int, SType ())) -> GenM ()
        updateLoc (s, (_, t)) = do
            let vi = ValIdent s
            mbcol <- asks (Map.lookup vi . regAlloc . regs)
            case mbcol of
                Just reg_ -> do
                    modify (\st -> st{vars = Map.insert vi (VarS vi t (LocReg reg_)) (vars st)})
                Nothing -> return ()

getLoc :: ValIdent -> GenM Loc
getLoc vi = do
    mbvar <- gets (Map.lookup vi . vars)
    case mbvar of
        Just var -> return $ varLoc var
        Nothing  -> error $ "internal error. value not found " ++ toStr vi

getValLoc :: Val a -> GenM Loc
getValLoc val = case val of
    VInt _ n    -> return $ LocImm (fromInteger n)
    VNegInt _ n -> return $ LocImm (fromInteger $ -n)
    VTrue _     -> return $ LocImm 1
    VFalse _    -> return $ LocImm 0
    VVal _ _ vi -> do
        varS <- getVarS vi
        return $ varLoc varS
    VNull {}    -> return $ LocImm 0

getPreservedRegs :: GenM [Reg]
getPreservedRegs = do
    l <- gets live
    cols <- asks (regAlloc . regs)
    let unchanged = HashMap.filterWithKey (\k _ -> not $ k `HashSet.member` liveKill l) (liveOut l)
        occupied = Map.filterWithKey (\vi _ -> toStr vi `HashMap.member` unchanged) cols
    traceM' $ "occupied regs: " ++ show occupied ++ ", because unchange = " ++ show unchanged
    return $ Map.elems occupied

newStrConst :: String -> GenM Const
newStrConst s = do
    (c, cs) <- gets (constsAdd s . consts)
    modify (\st -> st{consts = cs})
    return c

-- Generate a label in the context of the current method.
label :: LabIdent -> GenM LabIdent
label l = asks (`labelGen` l)

setStack :: Stack -> GenM ()
setStack s = modify (\st -> st {stack = s})

-- Get the description of a variable.
getVarS :: ValIdent -> GenM VarState
getVarS vi = do
    mb <- gets (Map.lookup vi . vars)
    case mb of
        Nothing -> error $ "internal error. no varS for var " ++ show vi
        Just g  -> return g

-- Get class metadata.
getClass :: SymIdent -> GenM CompiledClass
getClass i = do
    mb <- asks (Map.lookup i . classes)
    case mb of
        Nothing -> error $ "internal error. no class " ++ toStr i
        Just cl -> return cl

-- Get the size of a variable.
varSize :: VarState -> Size
varSize varS = typeSize $ varType varS

-- Is the variable currently alive.
isLive :: ValIdent -> GenM Bool
isLive (ValIdent vi) = do
    l <- gets live
    return $ HashMap.member vi $ liveIn l

-- Debug

fullTrace :: GenM ()
fullTrace = do
    l <- gets live
    traceM' ("live: " ++ show (HashMap.keys $ liveIn l))
    varSs <- gets (Map.elems . vars)
    s <- gets stack
    traceM' ("stack: " ++ show (stackReservedSize s) ++ " + " ++ show (stackOverheadSize s))
    mapM_ (\vs -> traceM' ("value " ++ toStr (varName vs) ++ ", "
            ++ "type: " ++ show (varType vs)
            ++ " loc: " ++ show (varLoc vs))) varSs

traceM' :: String -> GenM ()
traceM' s = when traceEnabled (do
    idx <- gets traceIdx
    modify (\st -> st{traceIdx = idx + 1})
    traceM ("{" ++ show idx ++ "}  " ++ s)
    )
