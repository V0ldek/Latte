{-# LANGUAGE FlexibleInstances #-}
module X86_64.CodeGen.GenM (
      addLoc,
      fullTrace,
      getClass,
      getRegS,
      getValLoc,
      getValLocs,
      getValUnreservedReg,
      getVarS,
      isLive,
      label,
      newStrConst,
      newVar,
      newVarPtr,
      nonStackLoc,
      setLoc,
      setLocs,
      setRegS,
      setStack,
      setVarS,
      traceM',
      useReg,
      useVal,
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
import           Data.Int
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Debug.Trace
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Class
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit           as Emit
import           X86_64.CodeGen.Stack
import           X86_64.Loc
import           X86_64.Registers
import           X86_64.Size

traceEnabled :: Bool
traceEnabled = False

data VarState = VarS {
    varName    :: ValIdent,
    varType    :: SType (),
    -- Locations in which the value of this variable currently resides.
    varLocs    :: [Loc],
    -- All names this value is available under, including this valName.
    varAliases :: [ValIdent],
    -- Most values are VarImm. However, when accessing fields or elements of an array
    -- a special VarPtr value is created, which has semantics of "the memory address in
    -- my varLocs offset by n"
    varKind    :: VarKind
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
    -- All registers used in the current method.
    usedRegs :: Set.Set Reg,
    -- Descriptions of registers.
    regs     :: Map.Map Reg RegState,
    -- Descriptions of variables.
    vars     :: Map.Map ValIdent VarState,
    -- Currently live variables and their next usage.
    live     :: NextUse,
    traceIdx :: Integer -- debug
}

data Env = Env {
    -- Generator of labels for the current method.
    labelGen :: LabIdent -> LabIdent,

    -- Liveness data for the current instruction.
    liveness :: Liveness,
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

-- Update the variable description to reflect that
-- it can be found in the given location.
addLoc :: Loc -> VarState -> VarState
addLoc loc varS  = case varKind varS of
    VarImm   -> varS {varLocs = loc:varLocs varS}
    VarPtr n -> varS {varLocs = LocPtr loc n:varLocs varS}

-- Update the variable description to reflect that
-- it can be found _only_ in the given location.
setLoc :: Loc -> VarState -> VarState
setLoc loc varS  = case varKind varS of
    VarImm   -> varS {varLocs = [loc]}
    VarPtr n -> varS {varLocs = [LocPtr loc n]}

-- Update the variable description to reflect that
-- it can be found _only_ in the given locations.
setLocs :: [Loc] -> VarState -> VarState
setLocs locs varS = foldr addLoc varS {varLocs = []} locs

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

-- Mark the variable as already used in this instruction,
-- possibly freeing it if it is dead after the current instruction.
useVar :: ValIdent -> GenM ()
useVar vi = do
    l <- asks liveness
    unless (toStr vi `Map.member` liveOut l) (do
        modify (\st -> st {live = Map.delete (toStr vi) (live st)})
        free vi)

-- Mark the value as already used in this instruction,
-- possibly freeing the associated variable if it is dead after the
-- current instruction.
useVal :: Val a -> GenM ()
useVal val = case val of
    VVal _ _ vi -> useVar vi
    _           -> return ()

-- Remove the variable from all locations it is in.
free :: ValIdent -> GenM ()
free vi = do
    mbvarS <- lookupVarS vi
    case mbvarS of
        Just varS -> do
            forM_ (varLocs varS) (freeFromLoc varS)
            varsMap <- gets vars
            let varsMap' = Map.map (\vs -> vs {varAliases = filter (/= vi) (varAliases vs)}) $
                    Map.adjust (\vs -> vs {varAliases = [vi], varLocs = []}) vi varsMap
            modify (\st -> st {vars = varsMap'})
        Nothing -> return ()

-- Remove the variable from a given location.
freeFromLoc :: VarState -> Loc -> GenM ()
freeFromLoc varS loc = case loc of
    LocPtr loc' _ -> freeFromLoc varS loc'
    LocReg reg_ -> do
        regS <- getRegS reg_
        let regS' = regS {regVals = filter (/= varName varS) (regVals regS)}
        setRegS regS'
    LocStack _ -> do
        s <- gets stack
        let s' = stackDelete (varName varS) s
        modify (\st -> st {stack = s'})
    _ -> return ()

-- Get the description of a variable.
getVarS :: ValIdent -> GenM VarState
getVarS vi = do
    mb <- gets (Map.lookup vi . vars)
    case mb of
        Nothing -> error $ "internal error. no varS for var " ++ show vi
        Just g  -> return g

-- Lookup the description of a variable.
lookupVarS :: ValIdent -> GenM (Maybe VarState)
lookupVarS vi = gets (Map.lookup vi . vars)

-- Get the description of a register.
getRegS :: Reg -> GenM RegState
getRegS reg_ = do
    mb <- gets (Map.lookup reg_ . regs)
    case mb of
        Nothing -> error $ "internal error. no regS for reg " ++ show reg_
        Just g  -> return g

-- Update the description of a variable.
setVarS :: VarState -> GenM ()
setVarS varS = modify (\st -> st {vars = Map.insert (varName varS) varS (vars st)})

-- Update the description of a register.
setRegS :: RegState -> GenM ()
setRegS regS = modify (\st -> st {regs = Map.insert (reg regS) regS (regs st)})

-- Get class metadata.
getClass :: SymIdent -> GenM CompiledClass
getClass i = do
    mb <- asks (Map.lookup i . classes)
    case mb of
        Nothing -> error $ "internal error. no class " ++ toStr i
        Just cl -> return cl

-- Get any non-stack location of a variable.
nonStackLoc :: VarState -> Maybe Loc
nonStackLoc varS = find isNonStack (varLocs varS)

-- Get the size of a variable.
varSize :: VarState -> Size
varSize varS = typeSize $ varType varS

-- Mark a register as used within this method
-- for the purposes of preserving callee-saved registers.
useReg :: Reg -> GenM RegState
useReg reg_ = do
    modify (\st -> st {usedRegs = Set.insert reg_ $ usedRegs st})
    mbregS <- gets (Map.lookup reg_ . regs)
    case mbregS of
        Just regS -> return regS
        Nothing   -> do
            let regS = initialRegs Map.! reg_
            setRegS regS
            return regS

-- Create a new variable of kind VarImm or completely kill it if it exists,
-- freeing it from all current locations.
newVar :: ValIdent -> SType () -> GenM ()
newVar vi t = do
    free vi
    l <- asks liveness
    case Map.lookup (toStr vi) (liveOut l) of
        Just next -> modify (\st -> st {live = Map.insert (toStr vi) next (live st)})
        Nothing   -> return ()
    setVarS $ VarS vi t [] [vi] VarImm

-- Create a new variable of kind VarPtr or completely kill it if it exists,
-- freeing it from all current locations.
newVarPtr :: ValIdent -> SType () -> Int64 -> GenM ()
newVarPtr vi t offset = do
    newVar vi t
    varS <- getVarS vi
    setVarS varS {varKind = VarPtr offset}

-- Is the variable currently alive.
isLive :: ValIdent -> GenM Bool
isLive (ValIdent vi) = gets $ Map.member vi . live

-- Get the location of a value.
-- If the value is a variable, the "best" location is returned,
-- i.e. immediates are preferred over registers, which are preferred
-- over memory locations.
getValLoc :: Val a -> GenM Loc
getValLoc val = case val of
    VInt _ n    -> return $ LocImm (fromInteger n)
    VNegInt _ n -> return $ LocImm (fromInteger $ -n)
    VTrue _     -> return $ LocImm 1
    VFalse _    -> return $ LocImm 0
    VVal _ _ vi -> do
        varS <- getVarS vi
        case sort $ varLocs varS of
            []  -> gets traceIdx >>= (\idx -> error $ "no locations for var " ++ toStr vi ++ " {" ++ show idx ++ "}")
            x:_ -> return x
    VNull {}    -> return $ LocImm 0

getValLocs :: Val a -> GenM [Loc]
getValLocs val = case val of
    VInt _ n    -> return [LocImm (fromInteger n)]
    VNegInt _ n -> return [LocImm (fromInteger $ -n)]
    VTrue _     -> return [LocImm 1]
    VFalse _    -> return [LocImm 0]
    VVal _ _ vi -> do
        varS <- getVarS vi
        return $ varLocs varS
    VNull {}    -> return [LocImm 0]

-- Try to get an unreserved register containing the given value.
getValUnreservedReg :: Val a -> GenM (Maybe Reg)
getValUnreservedReg val = case val of
    VVal _ _ vi -> do
        varS <- getVarS vi
        let regLocs = filter isReg (varLocs varS)
        regSs <- mapM (getRegS . asReg) regLocs
        return $ reg <$> find (not . regReserved) regSs
    _ -> return Nothing

-- Debug

fullTrace :: GenM ()
fullTrace = do
    l <- gets live
    traceM' ("live: " ++ show (Set.elems $ Map.keysSet $ l))
    varSs <- gets (Map.elems . vars)
    regSs <- gets (Map.elems . regs)
    s <- gets stack
    traceM' ("stack: " ++ show (Map.toList $ stackOccupiedSlots s) ++ ", " ++ show (stackReservedSize s) ++ " + " ++ show (stackOverheadSize s))
    mapM_ (\vs -> traceM' ("value " ++ toStr (varName vs) ++ ", "
            ++ "type: " ++ show (varType vs)
            ++ "aliases: " ++ intercalate ", " (map toStr (varAliases vs))
            ++ " locs: " ++ intercalate ", " (map show (varLocs vs)))) varSs
    mapM_ (\rs -> traceM' ("reg " ++ show (reg rs) ++ ", "
            ++ "values: " ++ show (regVals rs))) regSs

traceM' :: String -> GenM ()
traceM' s = when traceEnabled (do
    idx <- gets traceIdx
    modify (\st -> st{traceIdx = idx + 1})
    traceM ("{" ++ show idx ++ "}  " ++ s)
    )
