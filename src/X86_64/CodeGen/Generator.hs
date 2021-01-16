-- The core assembly codegen module.
-- Many debug traces are included in this code, controlled by the switch
-- traceEnabled in X86_64.CodeGen.GenM.
{-# LANGUAGE FlexibleInstances #-}
module  X86_64.CodeGen.Generator (generate) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                    (Bifunctor (second))
import           Data.Int
import           Data.List                         (partition)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Espresso.ControlFlow.CFG          (CFG (..), Node (..))
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Espresso.Types                    (deref, isInt, isStr,
                                                    valType, strType)
import           Identifiers
import           Utilities                         (isPowerOfTwo, log2, single)
import           X86_64.Class
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit               as Emit
import           X86_64.CodeGen.Epilogue
import           X86_64.CodeGen.GenM
import           X86_64.CodeGen.Module
import           X86_64.CodeGen.Prologue
import           X86_64.CodeGen.RegisterAllocation
import           X86_64.CodeGen.Stack
import           X86_64.Loc
import           X86_64.Registers
import           X86_64.Size

generate :: Metadata () -> [(CFG Liveness, Method a)] -> String
generate (Meta () clDefs) mthds =
    let (mthds', cs) = foldr go ([], constsEmpty) mthds
    in  generateModule cls mthds' cs
    where
    cls = map compileClass clDefs
    clMap = Map.fromList $ map (\cl -> (clName cl, cl)) cls
    go (cfg@(CFG g), Mthd _ _ qi ps _) (xs, cs) =
        let initStack = stackReserve (map (second typeSize) locals) stackEmpty
            initState = St [] [] cs initStack Set.empty initialRegs Map.empty Map.empty 0
            st = runReader (execStateT goOne initState) (Env (labelFor qi) emptyLiveness clMap)
            rawMthd = CmpMthd (toStr $ labelFor qi entryLabel) [] (reverse $ allCode st) []
            mthd = withEpilogue st $ withPrologue qi st rawMthd
        in (mthd:xs, consts st)
        where
            goOne = do
                traceM' ("========== starting method: " ++ toStr (labelFor qi (LabIdent "")))
                traceM' (show locals)
                forM_ locals (uncurry newVar)
                forM_ (map fst locals) reserveLocal
                addParams ps
                let nodes = Map.elems g
                    entryNode = single $ filter ((== entryLabel) . nodeLabel) nodes
                    exitNode = single $ filter (any isRet . nodeCode) nodes
                    otherNodes = filter ((\l -> l /= nodeLabel entryNode && l /= nodeLabel exitNode) . nodeLabel) nodes
                genNode entryNode
                mapM_ genNode otherNodes
                when (nodeLabel entryNode /= nodeLabel exitNode) (genNode exitNode)
            genNode node = do
                traceM' ("===== starting block: " ++ toStr (nodeLabel node))
                mapM_ genInstr (nodeCode node)
            locals = Map.toList $ persistedLocals cfg
            reserveLocal vi = do
                s <- gets stack
                varS <- getVarS vi
                let (loc, s') = stackInsertReserved vi s
                    varS' = setLoc loc varS
                setVarS varS'
                setStack s'
            isRet instr = case instr of
                IRet _ _ -> True
                IVRet _  -> True
                _        -> False

-- Set the descriptions for method parameters.
addParams :: [Param a] -> GenM ()
addParams ps = mapM_ (uncurry addParam) (zip ps [0..])
    where
        addParam (Param _ t vi) idx = do
            let loc = argLoc idx
            _ <- newVar vi (() <$ t)
            case loc of
                LocReg reg_ -> saveInReg vi reg_
                LocStack {} -> do
                    varS <- getVarS vi
                    let varS' = setLoc loc varS
                    setVarS varS'
                _ -> error $ "addParams: invalid location from argLoc " ++ show loc

{-
czemu  tak   trudno
assembler generować
termin    nadchodzi
-}
genInstr :: Instr Liveness -> GenM ()
genInstr instr =
    let instrLiveness = single instr
    in local (\env -> env {liveness = instrLiveness}) (do
        modify (\st -> st {live = liveIn instrLiveness})
        traceM' (show instr)
        fullTrace
        case instr of
            ILabel _ l -> do
                l' <- label l
                Emit.label l' ""
            ILabelAnn _ l f t -> do
                l' <- label l
                Emit.label l' $ "lines " ++ show f ++ "-" ++ show t
            IVRet _ -> do
                resetStack
                endBlock
            IRet _ val -> do
                moveToReg val rax
                resetStack
                endBlock
            IOp _ vi v1 op v2 -> case op of
                OpAdd _ | isInt (valType v1) -> emitSimpleBin (\l1 l2 -> Emit.add l1 l2 "") vi v1 v2
                OpAdd _ | isStr (valType v1) -> do
                    genCall (CallDirect "lat_cat_strings") [v1, v2]
                    newVar vi (Ref () strType)
                    saveInReg vi rax
                    useVal v1
                    useVal v2
                OpAdd _ -> error "internal error. invalid operand types for add."
                OpSub _ -> emitSimpleBin Emit.sub vi v1 v2
                OpMul _ -> do
                    loc1 <- getValLoc v1
                    loc2 <- getValLoc v2
                    case (loc1, loc2) of
                        (LocImm n, _) | isPowerOfTwo n ->
                            useVal v1 >> emitBitShift (\r ->
                                Emit.sal (log2 n) r ("multiply by " ++ show n)) vi v2
                        (_, LocImm n) | isPowerOfTwo n ->
                            useVal v2 >> emitBitShift (\r ->
                                Emit.sal (log2 n) r ("multiply by " ++ show n)) vi v1
                        _ -> emitSimpleBin Emit.imul vi v1 v2
                OpDiv _ -> do
                    locRhs <- getValLoc v2
                    case locRhs of
                        LocImm n | isPowerOfTwo n ->
                            useVal v2 >> emitBitShift (\r ->
                                Emit.sar (log2 n) r ("divide by " ++ show n)) vi v1
                        _ -> emitDivBin rax vi v1 v2
                OpMod _ -> do
                    locRhs <- getValLoc v2
                    case locRhs of
                        LocImm n | isPowerOfTwo n -> do
                            -- n % 2^k
                            -- is the same as
                            -- n AND (2^k - 1)
                            useVal v2
                            reg_ <- moveToAnyReg v1
                            useVal v1
                            freeReg reg_
                            Emit.and Double (LocImm (n - 1)) (LocReg reg_) ("modulo by " ++ show n)
                            newVar vi (Int ())
                            saveInReg vi reg_
                        _ -> emitDivBin rdx vi v1 v2
                OpLTH _ -> emitCmpBin Emit.setl vi v1 v2
                OpLE _  -> emitCmpBin Emit.setle vi v1 v2
                OpGTH _ -> emitCmpBin Emit.setg vi v1 v2
                OpGE _  -> emitCmpBin Emit.setge vi v1 v2
                OpEQU _ -> emitCmpBin Emit.sete vi v1 v2
                OpNE _  -> emitCmpBin Emit.setne vi v1 v2
            ISet _ vi v -> do
                let t = () <$ valType v
                case v of
                    VVal _ _ othVi -> do
                        othVarS <- getVarS othVi
                        let othVarS' = othVarS {varAliases = vi:varAliases othVarS}
                            rs = map asReg $ filter isReg (varLocs othVarS')
                        regSs <- mapM getRegS rs
                        let regSs' = map (\r -> r {regVals = vi:regVals r}) regSs
                        mapM_ setRegS regSs'
                        setVarS othVarS'
                        newVar vi t
                        varS <- getVarS vi
                        setVarS varS {varLocs = varLocs othVarS', varAliases = varAliases othVarS', varKind = varKind othVarS'}
                    _ -> do
                        newVar vi t
                        varS <- getVarS vi
                        let loc = case v of
                                VInt _ n    -> LocImm (fromInteger n)
                                VNegInt _ n -> LocImm (fromInteger $ -n)
                                VTrue _     -> LocImm 1
                                VFalse _    -> LocImm 0
                                VNull {}    -> LocImm 0
                                VVal {}     -> error "impossible"
                            varS' = setLoc loc varS
                        setVarS varS'
                useVal v
            IStr _ vi str -> do
                let len = toInteger $ length str
                    t = Ref () $ strType
                strConst <- newStrConst str
                reg_ <- moveConstToAnyReg strConst
                newVar vi t
                saveInReg vi reg_
                genCall (CallDirect "lat_new_string") [VVal () t vi, VInt () len]
                newVar vi t
                saveInReg vi rax
            IUnOp _ vi op v -> case op of
                UnOpNeg _ -> do
                    let t = () <$ valType v
                    case v of
                        VVal {} -> do
                            reg_ <- moveToAnyReg v
                            useVal v
                            freeReg reg_
                            Emit.neg reg_
                            newVar vi t
                            saveInReg vi reg_
                        _ -> do
                            newVar vi t
                            varS <- getVarS vi
                            setVarS $ case v of
                                VInt _ n -> setLoc (LocImm (fromInteger (-n))) varS
                                VNegInt _ n -> setLoc (LocImm (fromInteger n)) varS
                                _ -> error "internal error. invalid operand to UnOpNeg."
                UnOpNot _ -> do
                    let t = () <$ valType v
                    case v of
                        VVal {} -> do
                            reg_ <- moveToAnyReg v
                            useVal v
                            freeReg reg_
                            Emit.xor Byte (LocImm 1) (LocReg reg_)
                            newVar vi t
                            saveInReg vi reg_
                        _ -> do
                            newVar vi t
                            varS <- getVarS vi
                            setVarS $ case v of
                                VTrue _ -> setLoc (LocImm 0) varS
                                VFalse _ -> setLoc (LocImm 1) varS
                                _ -> error "internal error. invalid operand to UnOpNot"
            IVCall _ call -> case call of
                    Call _ _ qi args     -> genCall (CallDirect $ getCallTarget qi) args
                    CallVirt _ _ qi args -> genCallVirt qi args
            ICall _ vi call -> do
                t <- case call of
                        Call _ t' qi args     -> genCall (CallDirect $ getCallTarget qi) args >> return t'
                        CallVirt _ t' qi args -> genCallVirt qi args >> return t'
                newVar vi (() <$ t)
                saveInReg vi rax
            ILoad _ vi val -> do
                loc <- getValLoc val
                case loc of
                    LocPtr _ offset -> do
                        let t = () <$ deref (valType val)
                        reg_ <- moveToAnyReg val
                        useVal val
                        freeReg reg_
                        traceM' $ "Emitting load " ++ toStr vi ++ " of type " ++ show t
                        Emit.movFromMemToReg (typeSize t) reg_ offset reg_ ("load " ++ toStr vi)
                        newVar vi t
                        saveInReg vi reg_
                    _ -> error $ "internal error. load on nonptr location " ++ show loc
            IStore _ v1 v2 -> do
                loc <- getValLoc v2
                case loc of
                    LocPtr _ offset -> do
                        let t = valType v1
                        srcReg <- moveToAnyReg v1
                        useVal v1
                        freeReg srcReg
                        reserveReg srcReg
                        destPtrReg <- moveToAnyReg v2
                        useVal v2
                        unreserveReg srcReg
                        Emit.movFromRegToMem (typeSize t) srcReg destPtrReg offset
                    _ -> error $ "internal error. store on nonptr location " ++ show loc
            INew _ vi t -> case t of
                Cl _ clIdent -> do
                    cl <- getClass clIdent
                    let sizeArg = VInt () (toInteger $ clSize cl)
                    genCall (CallDirect "lat_new_instance") [sizeArg]
                    newVar vi (Ref () (() <$ t))
                    saveInReg vi rax
                    let (LocReg reg_) = argLoc 0
                    Emit.leaOfConst (toStr $ vTableLabIdent clIdent) reg_
                    Emit.movFromRegToMem Quadruple reg_ rax 0
                _ -> error $ "internal error. new on nonclass " ++ show t
            INewArr _ vi t val -> do
                let sizeArg = VInt () (toInteger $ sizeInBytes $ typeSize t)
                genCall (CallDirect "lat_new_array") [() <$ val, sizeArg]
                newVar vi (Ref () $ Arr () $ () <$ t)
                saveInReg vi rax
            IJmp _ li -> do
                li' <- label li
                resetStack
                saveBetweenBlocks
                endBlock
                Emit.jmp li'
            ICondJmp _ v l1 l2 -> do
                loc <- getValLoc v
                l1' <- label l1
                l2' <- label l2
                resetStack
                case loc of
                    LocImm 0 -> do
                        saveBetweenBlocks
                        endBlock
                        Emit.jmp l2'
                    LocImm 1 -> do
                        saveBetweenBlocks
                        endBlock
                        Emit.jmp l1'
                    _ -> do
                        Emit.test Byte loc loc
                        saveBetweenBlocks
                        endBlock
                        Emit.jz l2'
                        Emit.jmp l1'
            IFld _ vi val (QIdent _ cli fldi) -> do
                cl <- getClass cli
                case Map.lookup fldi (clFlds cl) of
                    Just fld -> do
                        newVarPtr vi (Ref () (fldType fld)) (fldOffset fld)
                        locs <- getValLocs val
                        forM_ locs (saveInLoc vi)
                        useVal val
                    Nothing -> error $ "internal error. no such field " ++ toStr cli ++ "." ++ toStr fldi
            IArr _ vi v1 v2 -> do
                let t = valType v1
                case deref t of
                    Arr _ elemT -> do
                        let elemSize = typeSize elemT
                            idxOffset = if elemSize < Double
                                          then sizeInBytes Double `div` sizeInBytes elemSize
                                          else 1
                        arrReg <- moveToAnyReg v1
                        reserveReg arrReg
                        idxReg <- moveToAnyReg v2
                        unreserveReg arrReg
                        useVal v1
                        useVal v2
                        freeReg idxReg
                        Emit.add (LocImm64 idxOffset) (LocReg idxReg) "offset the array length field"
                        Emit.lea arrReg 0 idxReg elemSize (LocReg idxReg) ("address of " ++ toStr vi)
                        newVarPtr vi (Ref () $ () <$ elemT) 0
                        saveInReg vi idxReg
                    _ -> error $ "internal error. invalid type in IArr " ++ show t
            IArrLen _ vi val -> do
                reg_ <- moveToAnyReg val
                freeReg reg_
                useVal val
                Emit.movFromMemToReg Double reg_ 0 reg_ ("load array length " ++ toStr vi)
                newVar vi (Int ())
                saveInReg vi reg_
            IPhi {} -> error "internal error. phi should be eliminated before assembly codegen"
        fullTrace
        return ()
    )

data CallTarget = CallDirect String | CallAddress Reg Int64 String

genCall :: CallTarget -> [Val a] -> GenM ()
genCall target args = do
        let argsWithLocs = zip args (map argLoc [0..])
            (argsWithLocReg,  argsWithLocStack) = partition (isReg . snd) argsWithLocs
            argsInRegs = map (second asReg) argsWithLocReg
            argsOnStack = map fst argsWithLocStack
        callerSavedRegs <- gets (filter (\r -> regType r == CallerSaved) . Map.keys . regs)
        forM_ argsInRegs (uncurry passInReg)
        forM_ callerSavedRegs reserveReg
        forM_ callerSavedRegs freeReg
        forM_ callerSavedRegs unreserveReg
        stackBefore <- gets (stackOverheadSize . stack)
        locs <- mapM prepOnStack (reverse argsOnStack)
        alignStack
        stackAfter <- gets (stackOverheadSize . stack)
        forM_ locs (`Emit.push` "passing arg")
        case target of
            CallDirect l              -> Emit.callDirect l
            CallAddress reg_ offset s -> Emit.callAddress reg_ offset ("call " ++ s)
        Emit.decrStack (stackAfter - stackBefore)
        modify (\st -> st{stack = (stack st){stackOverheadSize = stackBefore}})
    where passInReg val reg_ = moveToReg val reg_ >> useVal val
          prepOnStack val = do
              s <- gets stack
              loc <- getValLoc val
              let s' = stackPushUnnamed Quadruple s
              setStack s'
              useVal val
              return loc
          alignStack = do
              (misalignment, s) <- gets (stackAlign16 . stack)
              Emit.incrStack misalignment "16 bytes alignment"
              setStack s

genCallVirt :: QIdent a -> [Val a] -> GenM ()
genCallVirt _ [] = error "internal error. callvirt with no args"
genCallVirt (QIdent _ cli i) args@(self:_) = do
    cl <- getClass cli
    let LocReg reg_ = argLoc 0
        offset = case Map.lookup i $ vtabMthds $ clVTable cl of
                    Just (_, n) -> n
                    Nothing     -> error ""
    moveToReg self reg_
    Emit.test Quadruple (LocReg reg_) (LocReg reg_)
    Emit.jz nullrefLabel
    freeReg rax
    reserveReg rax
    Emit.movFromMemToReg Quadruple reg_ 0 rax "load address of vtable"
    genCall (CallAddress rax offset (toStr i)) args
    unreserveReg rax

emitSimpleBin :: (Loc -> Loc -> GenM ()) -> ValIdent -> Val a -> Val a -> GenM ()
emitSimpleBin emitter vi v1 v2 = do
    reg_ <- moveToAnyReg v1
    loc2 <- getValLoc v2
    useVal v1
    useVal v2
    freeReg reg_
    emitter loc2 (LocReg reg_)
    newVar vi (Int ())
    saveInReg vi reg_

emitBitShift :: (Reg -> GenM ()) -> ValIdent -> Val a -> GenM ()
emitBitShift emitter vi val = do
    reg_ <- moveToAnyReg val
    useVal val
    freeReg reg_
    emitter reg_
    newVar vi (Int ())
    saveInReg vi reg_

emitCmpBin :: (Reg -> GenM ()) -> ValIdent -> Val a -> Val a -> GenM ()
emitCmpBin emitter vi v1 v2 = do
    reg_ <- moveToAnyReg v1
    loc2 <- getValLoc v2
    useVal v1
    useVal v2
    freeReg reg_
    Emit.cmp (valSize v1) loc2 (LocReg reg_)
    emitter reg_
    newVar vi (Bool ())
    saveInReg vi reg_

emitDivBin :: Reg -> ValIdent -> Val a -> Val a -> GenM ()
emitDivBin resultReg_ vi v1 v2 = do
    moveToReg v1 rax
    reserveReg rax
    reserveReg rdx
    useVal v1
    freeReg rax
    freeReg rdx
    unreserveReg rax
    unreserveReg rdx
    loc2 <- materialise v2
    useVal v2
    Emit.cdq
    Emit.idiv Double loc2
    newVar vi (Int ())
    saveInReg vi resultReg_

resetStack :: GenM ()
resetStack = do
    s <- gets stack
    let (n, s') = stackClearOverhead s
    Emit.decrStack n
    setStack s'

-- Set stack and descriptions to a state where all locals are saved on stack
-- and nothing else is stored anywhere.
endBlock :: GenM ()
endBlock = do
    s <- gets stack
    let locals = stackReservedSlots s
    varSs <- gets vars
    let varSs' = Map.mapWithKey (\vi slot -> setLoc (slotToLoc slot) VarS {
                varName = vi,
                varType = varType $ varSs Map.! vi,
                varAliases = [vi],
                varLocs = [],
                varKind = varKind $ varSs Map.! vi}) locals
        s'     = foldr (\vi x -> snd $ stackInsertReserved vi x) s (Map.keys locals)
    modify (\st -> st {allCode = bbCode st ++ allCode st,
                       bbCode = [],
                       regs = initialRegs,
                       stack = s',
                       vars = varSs'})
