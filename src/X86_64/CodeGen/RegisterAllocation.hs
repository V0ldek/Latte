module X86_64.CodeGen.RegisterAllocation where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List                     (minimumBy)
import qualified Data.Map                      as Map
import           Data.Ord                      (comparing)
import qualified Data.Set                      as Set
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit           as Emit
import           X86_64.CodeGen.GenM
import           X86_64.CodeGen.Stack
import           X86_64.Loc
import           X86_64.Registers
import           X86_64.Size

-- Get all locals that need to be persisted between basic blocks based on
-- CFG liveness data.
persistedLocals :: CFG Liveness -> Map.Map ValIdent (SType ())
persistedLocals (CFG g) =
    let sizes = foldr seekSize Map.empty (concatMap nodeCode (Map.elems g))
        result = Map.restrictKeys sizes vis
    in if Map.keysSet result /= vis
        then error "internal error. missing sizes in persisted locals"
        else Map.map (() <$) result
    where
        vis = Map.foldr (Set.union . nodePersisted) Set.empty g
        nodePersisted node = Set.map ValIdent $ Map.keysSet $ liveOut (nodeTail node)
        seekSize instr m = case instr of
            IRet _ val          -> addVal val m
            IOp _ _ val1 _ val2 -> addVal val2 (addVal val1 m)
            ISet _ _ val        -> addVal val m
            IUnOp _ _ _ val     -> addVal val m
            IVCall _ call       -> addCall call m
            ICall _ _ call      -> addCall call m
            ICondJmp _ val _ _  -> addVal val m
            ILoad _ _ val       -> addVal val m
            IStore _ val1 val2  -> addVal val2 (addVal val1 m)
            IFld _ _ val _      -> addVal val m
            IArr _ _ val1 val2  -> addVal val2 (addVal val1 m)
            _                   -> m
        addVal val m = case val of
            VVal _ t vi -> Map.insert vi t m
            _           -> m
        addCall (Call _ _ _ vs) m     = foldr addVal m vs
        addCall (CallVirt _ _ _ vs) m = foldr addVal m vs

-- Ensure the value is available in a register or in memory.
materialise :: Val a -> GenM Loc
materialise val = do
    loc <- getValLoc val
    case loc of
        LocStack {} -> return loc
        LocReg {}   -> return loc
        LocImm {}   -> LocReg <$> moveToAnyReg val

-- Free the register while preserving alive values that had their only
-- location set as this register. May cause spilling if no other registers
-- are availbale.
freeReg :: Reg -> GenM ()
freeReg reg_ = do
    regS <- getRegS reg_
    reserveReg reg_
    forM_ (regVals regS) go
    setRegS (regS {regVals = []}) -- sets reserve state back
    where
        go vi = do
            secureValue vi
            varS <- getVarS vi
            let varS' = varS {varLocs = filter (\r -> r /= LocReg reg_) (varLocs varS)}
            setVarS varS'

-- Mark the register as reserved.
-- Reserved registers cannot be inserted into.
reserveReg :: Reg -> GenM ()
reserveReg reg_ = do
    regS <- getRegS reg_
    let regS' = regS {regReserved = True}
    setRegS regS'

-- Mark the register as not reserved.
unreserveReg :: Reg -> GenM ()
unreserveReg reg_ = do
    regS <- getRegS reg_
    let regS' = regS {regReserved = False}
    setRegS regS'

-- Ensure that the variable has a persisted location that is not a reserved
-- register if it is alive.
-- - If the value is on the stack, do nothing.
-- - If the value is only in reserved registers, move it to any unreserved register.
secureValue :: ValIdent -> GenM ()
secureValue vi = do
    alive <- isLive vi
    varS <- getVarS vi
    when (alive && all isNonStack (varLocs varS)) (do
        regSs <- mapM (getRegS . asReg) (filter isReg (varLocs varS))
        when (all regReserved regSs) (void $ moveToAnyReg (VVal () (varType varS) vi)))

-- Put the given variable into the register, replacing any values that were in it before.
saveInReg :: ValIdent -> Reg -> GenM ()
saveInReg vi reg_ = do
    regS <- useReg reg_
    freeReg reg_
    varS <- getVarS vi
    when (regReserved regS) (error $ "internal error. attempt to use a reserved register " ++ show reg_)
    let varS' = varS {varLocs = LocReg reg_ : varLocs varS}
        regS' = regS {regVals = varAliases varS}
    setVarS varS'
    setRegS regS'

-- Write all alive, yet unwritten locals persisted between blocks.
saveBetweenBlocks :: GenM ()
saveBetweenBlocks = do
    locals <- gets (map fst . stackReservedLocs . stack)
    liveLocals <- filterM isLive locals
    forM_ liveLocals (\vi -> unlessM (gets (stackContains vi . stack)) (saveOnStack vi))

-- Save the variable on the stack if it is not already there.
saveOnStack :: ValIdent -> GenM ()
saveOnStack vi = do
    varS <- getVarS vi
    s <- gets stack
    let mbsrcLoc = nonStackLoc varS
    (varS', s') <- case mbsrcLoc of
        Nothing
            | stackIsValueReserved vi s && not (stackContains vi s) -> do
                -- Value is already on stack, but not in its reserved local slot.
                let (loc@(LocStack n), s') = stackInsertReserved vi s
                reg_ <- moveToAnyReg (VVal () (varType varS) vi)
                Emit.movToStack (varSize varS) (LocReg reg_) n ("save " ++ toStr vi)
                return (varS {varLocs = loc : varLocs varS}, s')
        Nothing     -> return (varS, s)    -- Value is already on stack.
        Just srcLoc | stackIsValueReserved vi s -> do
            let (loc@(LocStack n), s') = stackInsertReserved vi s
            Emit.movToStack (varSize varS) srcLoc n ("save " ++ toStr vi )
            return (varS {varLocs = loc : varLocs varS}, s')
        Just srcLoc -> do
            let (loc, s') = stackPush vi Quadruple s
            Emit.push srcLoc ("spill " ++ toStr vi)
            return (varS {varLocs = loc : varLocs varS}, s')
    setVarS varS'
    setStack s'

-- Select the best unreserved register to put a value into.
chooseReg :: GenM Reg
chooseReg = do
    regSs <- gets (filter (not . regReserved) . Map.elems . regs)
    ranks <- mapM rankReg regSs
    traceM' ("Length of regSs: " ++ show (length regSs))
    return $ reg $ fst $ minimumBy (comparing snd) (zip regSs ranks)

-- Get the rank of the register based on its state.
rankReg :: RegState -> GenM RegRank
rankReg regS =
    if null $ regVals regS
      then return $ Free (regType $ reg regS)
      else do
        l <- gets live
        let nextUse = minimum $ map (getValUse l) (regVals regS)
        varSs <- mapM getVarS (regVals regS)
        return $ if all (any isStack . varLocs) varSs
                   then Clean nextUse
                   else Dirty nextUse
    where
        getValUse l vi =
            case Map.lookup (toStr vi) l of
                Just use -> use
                Nothing ->
                       error $ "internal error. dead variable " ++ toStr vi ++ " in reg " ++ show (reg regS)

-- Move the value to the register removing any values currently in it.
moveToReg :: Val a -> Reg -> GenM ()
moveToReg val reg_ = do
    freeReg reg_
    loc <- getValLoc val
    let size = valSize val
    comment <- case val of
        VVal _ _ vi -> saveInReg vi reg_ >>
            traceM' ("moving " ++ toStr vi ++ " to " ++ show reg_) >> return ("moving " ++ toStr vi)
        _           -> return ""
    Emit.movToReg size loc reg_ comment

-- Move the address of a string constant to an unreserved register.
moveConstToAnyReg :: Const -> GenM Reg
moveConstToAnyReg c = do
    reg_ <- chooseReg
    freeReg reg_
    Emit.leaOfConst c reg_
    return reg_

-- Move the value to any unreserved register if it is not already
-- in one. If it is, return that register.
moveToAnyReg :: Val a -> GenM Reg
moveToAnyReg val = do
    mbreg <- getValUnreservedReg val
    case mbreg of
        Just reg_ -> return reg_
        Nothing   -> do
            reg_ <- chooseReg
            regS <- getRegS reg_
            forM_ (regVals regS) saveOnStack
            moveToReg val reg_
            return reg_
