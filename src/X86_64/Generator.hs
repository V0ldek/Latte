{-# LANGUAGE FlexibleInstances #-}
module X86_64.Generator (generate, combineAssembly) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                      as Set
import           Debug.Trace
import           Espresso.ControlFlow.CFG      (CFG (..), Node (..), nodeTail)
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities
import           X86_64.Consts
import qualified X86_64.Emit                   as Emit
import           X86_64.Loc
import           X86_64.Registers
import           X86_64.Size
import           X86_64.Stack

traceEnabled :: Bool
traceEnabled = False

data CompiledMethod = CmpMthd {
    mthdEntry    :: String,
    mthdPrologue :: [String],
    mthdCode     :: [String],
    mthdEpilogue :: [String]
}

data VarState = VarS {varName :: ValIdent, varType :: SType (), varLocs :: [Loc], varAliases :: [ValIdent]}

data Store = St {
    allCode  :: [String],
    bbCode   :: [String],
    consts   :: ConstSet,
    stack    :: Stack,
    usedRegs :: Set.Set Reg,
    regs     :: Map.Map Reg RegState,
    vars     :: Map.Map ValIdent VarState,
    traceIdx :: Integer -- debug
}

data Env = Env {
    labelGen :: LabIdent -> LabIdent,
    liveness :: Liveness
}

type GenM = StateT Store (Reader Env)

instance Emit.EmitM GenM where
    emit s = modify (\st -> st {bbCode = s:bbCode st})

newStrConst :: String -> GenM String
newStrConst s = do
    (c, cs) <- gets (constsAdd s . consts)
    modify (\st -> st{consts = cs})
    return (constIdent c)

combineAssembly :: [CompiledMethod] -> ConstSet -> String
combineAssembly mthds allConsts =
       let code = concatMap (\m -> emitMthd m ++ "\n") mthds
           constsCode = unlines $
               map (\c -> constIdent c ++ ":\n  .string " ++ show (constValue c)) (constsElems allConsts)
           nativeExterns = unlines $ map (".extern " ++ ) [
                    "lat_print_int",
                    "lat_print_string",
                    "lat_read_int",
                    "lat_read_string",
                    "lat_error",
                    "lat_nullchk",
                    "lat_new_string",
                    "lat_cat_strings"
                ]
           globals = if any (\m -> mthdEntry m == mainEntry) mthds then ".global main\n" else ""
       in nativeExterns ++ globals ++ "\n" ++ constsCode ++ "\n\n" ++ code
    where mainEntry = toStr $
            labelFor (QIdent () (SymIdent $ toStr topLevelClassIdent) (SymIdent $ toStr mainSymIdent)) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code

generate :: [(CFG Liveness, Method a)] -> String
generate mthds =
    let (mthds', cs) = foldr go ([], constsEmpty) mthds
    in  combineAssembly mthds' cs
    where
    go (cfg@(CFG g), Mthd _ _ qi ps _) (xs, cs) =
        let initStack = stackReserve (map (second typeSize) locals) stackEmpty
            initState = St [] [] cs initStack Set.empty initialRegs Map.empty 0
            st = runReader (execStateT goOne initState) (Env (labelFor qi) emptyLiveness)
            rawMthd = CmpMthd (toStr $ labelFor qi entryLabel) [] (reverse $ allCode st) []
            mthd = withEpilogue st $ withPrologue qi st rawMthd
        in (mthd:xs, consts st)
        where
            goOne = do
                traceM' ("========== starting method: " ++ toStr (labelFor qi (LabIdent "")))
                traceM' (show locals)
                forM_ locals (uncurry newVal)
                forM_ (map fst locals) reserveLocal
                addParams ps
                let nodes = Map.elems g
                    entryNode = single $ filter ((== entryLabel) . nodeLabel) nodes
                    exitNode = single $ filter ((== exitLabel) . nodeLabel) nodes
                    otherNodes = filter ((\l -> l /= entryLabel && l /= exitLabel) . nodeLabel) nodes
                genNode entryNode
                mapM_ genNode otherNodes
                genNode exitNode
            genNode node = do
                traceM' ("===== starting block: " ++ toStr (nodeLabel node))
                mapM_ genInstr (nodeCode node)
            locals = Map.toList $ persistedLocals cfg
            reserveLocal vi = do
                s <- gets stack
                varS <- getVarS vi
                let (loc, s') = stackInsertReserved vi s
                    varS' = varS {varLocs = [loc]}
                setVarS varS'
                modify (\st -> st {stack = s'})

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

withPrologue :: QIdent a -> Store -> CompiledMethod -> CompiledMethod
withPrologue qi st mthd =
    let locs = stackReservedSize $ stack st
        savedRegs = sort $ filter (\r -> regType r == CalleeSaved) $ Set.elems $ usedRegs st
        needsAlignment = odd $ length savedRegs
        stackOffset = locs
        prologue = [
            Emit.sanitise (toStr (labelFor qi (LabIdent ""))) ++ ":"
            ] ++ map (\r -> "  push %" ++ reg64 r) savedRegs ++
            ["  subq $8, %rsp # 16 bytes alignment" | needsAlignment] ++
            ["  push %rbp",
             "  movq %rsp, %rbp",
             "  subq $" ++ show stackOffset ++ ", %rsp"]
        -- Access to parameters passed on stack has to be offset by 8 for each saved
        -- register, including rbp, and then additional 8 for the return address.
        paramOffset = 8 * (length savedRegs + 1 + if needsAlignment then 1 else 0) + 8
        newCode = map (offsetParamsOnStack paramOffset) (mthdCode mthd)
        offsetParamsOnStack n line = reverse $ go [] line
            where
                -- bleh
                go xs (' ':rest) = let offset = takeWhile isDigit rest
                                       expr = dropWhile isDigit rest
                                   in if "(%rbp)" `isPrefixOf` expr && not (null offset)
                                        then go (reverse (show (read offset + n)) ++ " " ++ xs) expr
                                        else go (' ':xs) rest
                go xs (y:ys) = go (y:xs) ys
                go xs [] = xs
    in mthd {mthdPrologue = prologue, mthdCode = newCode}

withEpilogue :: Store -> CompiledMethod -> CompiledMethod
withEpilogue st mthd =
    let savedRegs = sortOn Down $ filter (\r -> regType r == CalleeSaved) $ Set.elems $ usedRegs st
        neededAlignment = odd $ length savedRegs
        epilogue = ["  leave"] ++
            ["  addq $8, %rsp # reset alignment " | neededAlignment] ++
            map (\r -> "  pop %" ++ reg64 r) savedRegs ++ [
                "  ret"
            ]
    in mthd {mthdEpilogue = epilogue}

addParams :: [Param a] -> GenM ()
addParams ps = mapM_ (uncurry addParam) (zip ps [0..])
    where
        addParam (Param _ t vi) idx = do
            let mbreg = argReg idx
            _ <- newVal vi (() <$ t)
            case mbreg of
                Just reg_ -> saveInReg vi reg_
                Nothing   -> do
                    varS <- getVarS vi
                    let -- Each argument takes 8 bytes, offset taken into account in withPrologue
                        stackLoc = LocStack $ (fromInteger idx - 6) * 8
                        varS' = varS {varLocs = [stackLoc]}
                    setVarS varS'

{-
data Instr a
    = ILabel a LabIdent
    | ILabelAnn a LabIdent Integer Integer
    | IVRet a
    | IRet a (Val a)
    | IOp a ValIdent (Val a) (Op a) (Val a)
    | ISet a ValIdent (Val a)
    | IUnOp a ValIdent (UnOp a) (Val a)
    | IVCall a (Call a)
    | ICall a ValIdent (Call a)
    | IJmp a LabIdent
    | ICondJmp a (Val a) LabIdent LabIdent
    | ILoad a ValIdent (Val a)
    | IStore a (Val a) (Val a)
    | IFld a ValIdent (Val a) (QIdent a)
    | IArr a ValIdent (Val a) (Val a)
    | IPhi a ValIdent [PhiVariant a]
  deriving (Eq, Ord, Show, Read, Foldable)
-}
{-
czemu tak trudno
assembler generować
termin nadchodzi
-}

emitSimpleBin :: (Loc -> Loc -> GenM ()) -> ValIdent -> Val a -> Val a -> GenM ()
emitSimpleBin emitter vi v1 v2 = do
    reg_ <- moveToAnyReg v1
    loc2 <- getValLoc v2
    freeNotAlive
    freeReg reg_
    emitter loc2 (LocReg reg_)
    newVal vi (Int ())
    saveInReg vi reg_

emitCmpBin :: (Reg -> GenM ()) -> ValIdent -> Val a -> Val a -> GenM ()
emitCmpBin emitter vi v1 v2 = do
    reg_ <- moveToAnyReg v1
    loc2 <- getValLoc v2
    freeNotAlive
    freeReg reg_
    Emit.cmp (valSize v1) loc2 (LocReg reg_)
    emitter reg_
    newVal vi (Bool ())
    saveInReg vi reg_

emitDivBin :: Reg -> ValIdent -> Val a -> Val a -> GenM ()
emitDivBin resultReg_ vi v1 v2 = do
    moveToReg v1 rax
    reserveReg rax
    reserveReg rdx
    freeNotAlive
    freeReg rax
    freeReg rdx
    unreserveReg rax
    unreserveReg rdx
    loc2 <- materialise v2
    Emit.cdq
    Emit.idiv Double loc2
    newVal vi (Int ())
    saveInReg vi resultReg_

genInstr :: Instr Liveness -> GenM ()
genInstr instr =
    let live = single instr
    in local (\env -> env {liveness = live}) (do
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
                OpAdd _ | isInt (valType v1) -> emitSimpleBin Emit.add vi v1 v2
                OpAdd _ | isStr (valType v1) -> do
                    genCall "lat_cat_strings" [v1, v2]
                    newVal vi (Ref () (Str ()))
                    saveInReg vi rax
                    freeNotAlive
                OpAdd _ -> error "internal error. invalid operand types for add."
                OpSub _ -> emitSimpleBin Emit.sub vi v1 v2
                OpMul _ -> emitSimpleBin Emit.imul vi v1 v2
                OpDiv _ -> emitDivBin rax vi v1 v2
                OpMod _ -> emitDivBin rdx vi v1 v2
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
                        newVal vi t
                        varS <- getVarS vi
                        setVarS varS {varLocs = varLocs othVarS', varAliases = varAliases othVarS'}
                    _ -> do
                        newVal vi t
                        varS <- getVarS vi
                        setVarS $ case v of
                            VInt _ n -> varS {varLocs = [LocImm (fromInteger n)]}
                            VNegInt _ n -> varS {varLocs = [LocImm (fromInteger $ -n)]}
                            VTrue _ -> varS {varLocs = [LocImm 1]}
                            VFalse _ -> varS {varLocs = [LocImm 0]}
                            VNull _ -> varS {varLocs = [LocImm 0]}
                            VVal {} -> error "impossible"
                freeNotAlive
            IStr _ vi str -> do
                let len = toInteger $ length str
                    t = Ref () (Str ())
                strConst <- newStrConst str
                reg_ <- moveConstToAnyReg strConst
                newVal vi t
                saveInReg vi reg_
                genCall "lat_new_string" [VVal () t vi, VInt () len]
                newVal vi t
                saveInReg vi rax
                freeNotAlive
            IUnOp _ vi op v -> case op of
                UnOpNeg _ -> do
                    let t = () <$ valType v
                    case v of
                        VVal {} -> do
                            reg_ <- moveToAnyReg v
                            freeNotAlive
                            freeReg reg_
                            Emit.neg reg_
                            newVal vi t
                            saveInReg vi reg_
                        _ -> do
                            newVal vi t
                            varS <- getVarS vi
                            setVarS $ case v of
                                VInt _ n -> varS {varLocs = [LocImm (fromInteger (-n))]}
                                VNegInt _ n -> varS {varLocs = [LocImm (fromInteger n)]}
                                _ -> error "internal error. invalid operand to UnOpNeg."
                    freeNotAlive
                UnOpNot _ -> do
                    let t = () <$ valType v
                    case v of
                        VVal {} -> do
                            reg_ <- moveToAnyReg v
                            freeNotAlive
                            freeReg reg_
                            Emit.xor Byte (LocImm 1) (LocReg reg_)
                            newVal vi t
                            saveInReg vi reg_
                        _ -> do
                            newVal vi t
                            varS <- getVarS vi
                            setVarS $ case v of
                                VTrue _ -> varS {varLocs = [LocImm 0]}
                                VFalse _ -> varS {varLocs = [LocImm 1]}
                                _ -> error "internal error. invalid operand to UnOpNot"
                    freeNotAlive
            IVCall _ call -> do
                case call of
                    Call _ _ qi args -> genCall (getCallTarget qi) args
                    CallVirt {}      -> error "callvirt unimplemented"
                freeNotAlive
            ICall _ vi call -> do
                t <- case call of
                        Call _ t' qi args -> genCall (getCallTarget qi) args >> return t'
                        CallVirt {}       -> error "callvirt unimplemented"
                newVal vi (() <$ t)
                saveInReg vi rax
                freeNotAlive
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
                        Emit.test loc loc
                        saveBetweenBlocks
                        endBlock
                        Emit.jz l2'
                        Emit.jmp l1'
            IJmp _ li -> do
                li' <- label li
                resetStack
                saveBetweenBlocks
                endBlock
                Emit.jmp li'
            IPhi {} -> error "internal error. phi should be eliminated before assembly codegen"
            _ -> error $ "unimplemented " ++ show instr
        fullTrace
        return ()
    )

genCall :: String -> [Val a] -> GenM ()
genCall target args = do
        let argsWithRegs = zip args (map argReg [0..])
            (args1, args2) = partition (isJust . snd) argsWithRegs
            argsInRegs = map (second fromJust) args1
            argsOnStack = map fst args2
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
        Emit.call target
        Emit.decrStack (stackAfter - stackBefore)
        modify (\st -> st{stack = (stack st){stackOverheadSize = stackBefore}})
    where passInReg val reg_ = moveToReg val reg_
          prepOnStack val = do
              s <- gets stack
              loc <- getValLoc val
              let s' = stackPushUnnamed Quadruple s -- TODO: make sure quadruple is necessary
              modify (\st -> st{stack = s'})
              return loc
          alignStack = do
              (misalignment, s) <- gets (stackAlign16 . stack)
              Emit.incrStack misalignment "16 bytes alignment"
              modify (\st -> st{stack = s})

getCallTarget :: QIdent a -> String
getCallTarget (QIdent _ (SymIdent i1) (SymIdent i2)) =
    if i1 == toStr topLevelClassIdent
    then case i2 of
        "readInt"     -> "lat_read_int"
        "readString"  -> "lat_read_string"
        "printInt"    -> "lat_print_int"
        "printString" -> "lat_print_string"
        "error"       -> "lat_error"
        _             -> i1 ++ "." ++ i2
    else i1 ++ "." ++ i2

freeNotAlive :: GenM ()
freeNotAlive = do
    live <- asks liveness
    vis <- gets (Map.keys . vars)
    let freeIfNotAlive vi = unless (Set.member (toStr vi) (Map.keysSet (liveOut live))) (free vi)
    forM_ vis freeIfNotAlive

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

freeFromLoc :: VarState -> Loc -> GenM ()
freeFromLoc varS loc = case loc of
    LocReg reg_ -> do
        regS <- getRegS reg_
        let regS' = regS {regVals = filter (/= varName varS) (regVals regS)}
        setRegS regS'
    LocStack _ -> do
        s <- gets stack
        let s' = stackDelete (varName varS) s
        modify (\st -> st {stack = s'})
    _ -> return ()

moveToReg :: Val a -> Reg -> GenM ()
moveToReg val reg_ = do
    freeReg reg_
    loc <- getValLoc val
    let size = valSize val
    comment <- case val of
        VVal _ _ vi -> saveInReg vi reg_ >> traceM' ("moving " ++ toStr vi ++ " to " ++ show reg_) >> return ("moving " ++ toStr vi)
        _           -> return ""
    Emit.movToReg size loc reg_ comment

moveConstToAnyReg :: String -> GenM Reg
moveConstToAnyReg c = do
    reg_ <- chooseReg
    freeReg reg_
    Emit.leaOfConst c reg_
    return reg_

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

chooseReg :: GenM Reg
chooseReg = do
    regSs <- gets (filter (not . regReserved) . Map.elems . regs)
    ranks <- mapM rankReg regSs
    traceM' ("Length of regSs: " ++ show (length regSs))
    return $ reg $ fst $ minimumBy (comparing snd) (zip regSs ranks)
    where rankReg regS = if null $ regVals regS then return $ Free (regType $ reg regS)
                         else do
                             live <- asks liveness
                             let nextUse = minimum $ map (getValUse live) (regVals regS)
                             varSs <- mapM getVarS (regVals regS)
                             return $ if all (any isStack . varLocs) varSs
                                        then Clean nextUse
                                        else Dirty nextUse
                         where
                           getValUse live vi =
                               case Map.lookup (toStr vi) (liveIn live) of
                                Just use -> use
                                Nothing ->
                                    error $ "internal error. dead variable " ++ toStr vi ++ " in reg " ++ show (reg regS)

isLiveDuring :: ValIdent -> GenM Bool
isLiveDuring vi = asks $ Map.member (toStr vi) . liveIn . liveness

isLiveAfter :: ValIdent -> GenM Bool
isLiveAfter vi = asks $ Map.member (toStr vi) . liveOut . liveness

materialise :: Val a -> GenM Loc
materialise val = do
    loc <- getValLoc val
    case loc of
        LocStack {} -> return loc
        LocReg {}   -> return loc
        LocImm {}   -> LocReg <$> moveToAnyReg val

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
    VNull _     -> return $ LocImm 0

getValUnreservedReg :: Val a -> GenM (Maybe Reg)
getValUnreservedReg val = case val of
    VVal _ _ vi -> do
        varS <- getVarS vi
        let regLocs = filter isReg (varLocs varS)
        regSs <- mapM (getRegS . asReg) regLocs
        return $ reg <$> find (not . regReserved) regSs
    _ -> return Nothing

reserveReg :: Reg -> GenM ()
reserveReg reg_ = do
    traceM' $ "reserving " ++ show reg_
    regS <- getRegS reg_
    let regS' = regS {regReserved = True}
    setRegS regS'

unreserveReg :: Reg -> GenM ()
unreserveReg reg_ = do
    traceM' $ "unreserving " ++ show reg_
    regS <- getRegS reg_
    let regS' = regS {regReserved = False}
    setRegS regS'

freeReg :: Reg -> GenM ()
freeReg reg_ = do
    regS <- getRegS reg_
    reserveReg reg_
    forM_ (regVals regS) go
    when (not $ regReserved regS) (traceM' $ "unreserving " ++ show reg_)
    setRegS (regS {regVals = []}) -- sets reserve state back
    where
        go vi = do
            secureValue vi
            varS <- getVarS vi
            let varS' = varS {varLocs = filter (\r -> r /= LocReg reg_) (varLocs varS)}
            setVarS varS'

secureValue :: ValIdent -> GenM ()
secureValue vi = do
    isLive <- isLiveDuring vi
    varS <- getVarS vi
    when (isLive && all isReg (varLocs varS)) (void $ moveToAnyReg (VVal () (varType varS) vi))

labelFor :: QIdent a -> LabIdent -> LabIdent
labelFor (QIdent _ (SymIdent i1) (SymIdent i2)) (LabIdent l1) = LabIdent $ i1 ++ "." ++ i2 ++ l1

saveInReg :: ValIdent -> Reg -> GenM ()
saveInReg vi reg_ = do
    regS <- useReg reg_
    freeReg reg_
    varS <- getVarS vi
    when (regReserved regS) (error $ "internal error. attempt to use reserved register " ++ show reg_)
    let varS' = varS {varLocs = LocReg reg_ : varLocs varS}
        regS' = regS {regVals = varAliases varS}
    setVarS varS'
    setRegS regS'

newVal :: ValIdent -> SType () -> GenM ()
newVal vi t = do
    free vi
    setVarS $ VarS vi t [] [vi]

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

getVarS :: ValIdent -> GenM VarState
getVarS vi = do
    mb <- gets (Map.lookup vi . vars)
    case mb of
        Nothing -> error $ "internal error. no varS for var " ++ show vi
        Just g  -> return g

lookupVarS :: ValIdent -> GenM (Maybe VarState)
lookupVarS vi = gets (Map.lookup vi . vars)

getRegS :: Reg -> GenM RegState
getRegS reg_ = do
    mb <- gets (Map.lookup reg_ . regs)
    case mb of
        Nothing -> error $ "internal error. no regS for reg " ++ show reg_
        Just g  -> return g

setVarS :: VarState -> GenM ()
setVarS varS = modify (\st -> st {vars = Map.insert (varName varS) varS (vars st)})

setRegS :: RegState -> GenM ()
setRegS regS = modify (\st -> st {regs = Map.insert (reg regS) regS (regs st)})

saveBetweenBlocks :: GenM ()
saveBetweenBlocks = do
    locals <- gets (map fst . stackReservedLocs . stack)
    liveLocals <- filterM isLiveAfter locals
    forM_ liveLocals (\vi -> unlessM (gets (stackContains vi . stack)) (saveOnStack vi))

saveOnStack :: ValIdent -> GenM ()
saveOnStack vi = do
    varS <- getVarS vi
    s <- gets stack
    let mbsrcLoc = nonStackLoc varS
    case mbsrcLoc of
        Nothing
            | stackIsValueReserved vi s && not (stackContains vi s) -> do
                -- Value is already on stack, but not in its reserved local slot.
                let (loc@(LocStack n), s') = stackInsertReserved vi s
                reg_ <- moveToAnyReg (VVal () (varType varS) vi)
                Emit.movToStack (varSize varS) (LocReg reg_) n ("save " ++ toStr vi)
                let varS' = varS {varLocs = loc : varLocs varS}
                setVarS varS'
                modify (\st -> st{stack = s'})
        Nothing     -> return ()    -- Value is already on stack.
        Just srcLoc | stackIsValueReserved vi s -> do
            let (loc@(LocStack n), s') = stackInsertReserved vi s
            Emit.movToStack (varSize varS) srcLoc n ("save " ++ toStr vi )
            let varS' = varS {varLocs = loc : varLocs varS}
            setVarS varS'
            modify (\st -> st {stack = s'})
        Just srcLoc -> do
            let (loc, s') = stackPush vi Quadruple s
            Emit.push srcLoc ("spill " ++ toStr vi)
            let varS' = varS {varLocs = loc : varLocs varS}
            setVarS varS'
            modify (\st -> st {stack = s'})

nonStackLoc :: VarState -> Maybe Loc
nonStackLoc varS = find isNonStack (varLocs varS)

valSize :: Val a -> Size
valSize val = case val of
    VInt _  _    -> Double
    VNegInt _  _ -> Double
    VTrue _      -> Byte
    VFalse _     -> Byte
    VNull _      -> Quadruple
    VVal _ t _   -> typeSize t

valType :: Val a -> SType a
valType val = case val of
    VInt a _    -> Int a
    VNegInt a _ -> Int a
    VTrue a     -> Bool a
    VFalse a    -> Bool a
    VNull {}    -> error "fixmeee"
    VVal _ t _  -> t

varSize :: VarState -> Size
varSize varS = typeSize $ varType varS

label :: LabIdent -> GenM LabIdent
label l = asks (`labelGen` l)

resetStack :: GenM ()
resetStack = do
    s <- gets stack
    let (n, s') = stackClearOverhead s
    Emit.decrStack n
    modify (\st -> st {stack = s'})

endBlock :: GenM ()
endBlock = do
    locals <- gets (stackReservedSlots . stack)
    varSs <- gets vars
    let varSs' = Map.mapWithKey (\vi slot -> VarS {
                varName = vi,
                varType = varType $ varSs Map.! vi,
                varLocs = [slotToLoc slot],
                varAliases = [vi]}) locals
    modify (\st -> st {allCode = bbCode st ++ allCode st,
                       bbCode = [],
                       regs = initialRegs,
                       vars = varSs'})

-- Debug

fullTrace :: GenM ()
fullTrace = do
    live <- asks liveness
    traceM' ("live: " ++ show (Set.elems $ Map.keysSet $ liveOut live))
    varSs <- gets (Map.elems . vars)
    s <- gets stack
    traceM' ("stack: " ++ show (Map.toList $ stackOccupiedSlots s) ++ ", " ++ show (stackReservedSize s) ++ " + " ++ show (stackOverheadSize s))
    mapM_ (\vs -> traceM' ("value " ++ toStr (varName vs) ++ ", "
            ++ "aliases: " ++ intercalate ", " (map toStr (varAliases vs))
            ++ " locs: " ++ intercalate ", " (map show (varLocs vs)))) varSs

traceM' :: String -> GenM ()
traceM' s = when traceEnabled (do
    idx <- gets traceIdx
    modify (\st -> st{traceIdx = idx + 1})
    traceM ("{" ++ show idx ++ "}  " ++ s)
    )

isInt :: SType a -> Bool
isInt t = case deref t of
    Int _ -> True
    _     -> False

isStr :: SType a -> Bool
isStr t = case deref t of
    Str _ -> True
    _     -> False

deref :: SType a -> SType a
deref t = case t of
    Ref _ t' -> t'
    _        -> t
