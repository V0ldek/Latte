{-# LANGUAGE FlexibleInstances #-}
module X86_64.Generator (generate, combineAssembly) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                      as Set
import           Debug.Trace
import           Espresso.ControlFlow.CFG      (CFG (..), Node (..))
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities
import qualified X86_64.Emit                   as Emit
import           X86_64.Loc
import           X86_64.Registers
import           X86_64.Size
import           X86_64.Stack

data Const = Const {constIdent :: String, constValue :: String}

data CompiledMethod = CmpMthd {
    mthdEntry    :: String,
    mthdConsts   :: [Const],
    mthdPrologue :: [String],
    mthdCode     :: [String],
    mthdEpilogue :: [String]
}

data ValState = ValS {valName :: ValIdent, valSize :: Size, valLocs :: [Loc], valAliases :: [ValIdent]}

data Store = St {
    allCode  :: [String],
    bbCode   :: [String],
    constIdx :: Integer,
    consts   :: [Const],
    stack    :: Stack,
    usedRegs :: Set.Set Reg,
    regs     :: Map.Map Reg RegState,
    vals     :: Map.Map ValIdent ValState
}

data Env = Env {
    labelGen :: LabIdent -> LabIdent,
    liveness :: Liveness
}

type GenM = StateT Store (Reader Env)

instance Emit.EmitM GenM where
    emit s = modify (\st -> st {bbCode = s:bbCode st})

combineAssembly :: [CompiledMethod] -> String
combineAssembly mthds =
       let code = concatMap (\m -> emitMthd m ++ "\n") mthds
           nativeExterns = unlines $ map (".extern " ++ ) [
                "lat_print_int",
                "lat_print_string",
                "lat_read_int",
                "lat_read_string",
                "lat_error"
            ]
       in
       if any (\m -> mthdEntry m == mainEntry) mthds
       then nativeExterns ++ ".global main\n\n" ++ code
       else nativeExterns ++ "\n\n" ++ code
    where mainEntry = toStr $
            labelFor (QIdent () (SymIdent $ toStr topLevelClassIdent) (SymIdent $ toStr mainSymIdent)) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code

generate :: Method a -> CFG Liveness -> CompiledMethod
generate (Mthd _ _ qi ps _) (CFG cfg) =
    let initState = St [] [] 0 [] emptyStack Set.empty initialRegs Map.empty
        st = runReader (execStateT go initState) (Env (labelFor qi) emptyLiveness)
    in  CmpMthd (toStr $ labelFor qi entryLabel) (consts st) (prologue qi st) (reverse $ allCode st) (epilogue st)
    where
        go = do
            addParams ps
            mapM_ genNode (Map.elems cfg)
        genNode node = do
            mapM_ genInstr (nodeCode node)
            endBlock

{-
data ValState = ValS {valName :: ValIdent, valSize :: Size, valLocs :: [Loc], valAliases :: [ValIdent]}-}

prologue :: QIdent a -> Store -> [String]
prologue qi st =
    let locs = stackSize $ stack st
        savedRegs = sort $ filter (\r -> regType r == CalleeSaved) $ Set.elems $ usedRegs st
    in [
        Emit.sanitise (toStr (labelFor qi (LabIdent ""))) ++ ":",
        "  push %rbp",
        "  movq %rsp, %rbp"
    ] ++ map (\r -> "  push %" ++ regHigh r) savedRegs ++ [
        "  subq $" ++ show locs ++ ", %rsp"
    ]

epilogue :: Store -> [String]
epilogue st =
    let savedRegs = sortOn Down $ filter (\r -> regType r == CalleeSaved) $ Set.elems $ usedRegs st
    in map (\r -> "  pop %" ++ regHigh r) savedRegs ++ [
        "  leave",
        "  ret"
    ]

addParams :: [Param a] -> GenM ()
addParams ps = mapM_ (uncurry addParam) (zip ps [0..])
    where
        addParam (Param _ t vi) idx = do
            let size = typeSize t
                mbreg = argReg idx
            _ <- newVal vi size
            case mbreg of
                Just reg_ -> saveInReg vi reg_
                Nothing   -> do
                    valS <- getValS vi
                    let valS' = valS {valLocs = [LocStack (-fromInteger idx * 8)]}
                    setValS valS'

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
genInstr :: Instr Liveness -> GenM ()
genInstr instr =
    let live = single instr
    in local (\env -> env {liveness = live}) (do
        fullTrace
        case instr of
            ILabel _ l -> do
                l' <- label l
                Emit.label l' ""
            ILabelAnn _ l f t -> do
                l' <- label l
                Emit.label l' $ "lines " ++ show f ++ "-" ++ show t
            IVRet _ -> return ()
            IRet _ val -> do
                moveToReg val rax
                reserveReg rax
                freeNotAlive
            IOp _ vi v1 op v2 -> case op of
                OpAdd _ -> do
                    newVal vi Double
                    reg_ <- moveToAnyReg v1
                    loc2 <- getValLoc v2
                    valS <- getValS vi
                    freeNotAlive
                    freeReg reg_
                    Emit.add loc2 (LocReg reg_)
                    let valS' = valS {valLocs = [LocReg reg_]}
                    setValS valS'
                _       -> error $ "unimplemented" ++ show instr
            ISet _ vi v -> do
                size <- getValSize v
                newVal vi size
                valS <- getValS vi
                valS' <- case v of
                    VInt _ n -> return $ valS {valLocs = [LocImm (fromInteger n)]}
                    VNegInt _ n -> return $ valS {valLocs = [LocImm (fromInteger $ -n)]}
                    VTrue _ -> return $ valS {valLocs = [LocImm 1]}
                    VFalse _ -> return $ valS {valLocs = [LocImm 0]}
                    VVal _ _ othVi -> do
                        othValS <- getValS othVi
                        let othValS' = othValS {valAliases = vi:valAliases othValS}
                            rs = map asReg $ filter isReg (valLocs othValS')
                        regSs <- mapM getRegS rs
                        let regSs' = map (\r -> r {regVals = vi:regVals r}) regSs
                        mapM_ setRegS regSs'
                        setValS othValS'
                        return $ valS {valLocs = valLocs othValS', valAliases = valAliases othValS'}
                    _ -> error "lalala"
                setValS valS'
                freeNotAlive
            IVCall _ call -> do
                genCall call
                freeNotAlive
            ICall _ vi call -> do
                genCall call
                -- TODO: size
                newVal vi Quadruple
                saveInReg vi rax
                freeNotAlive
            ICondJmp _ v l1 l2 -> do
                loc <- getValLoc v
                l1' <- label l1
                l2' <- label l2
                Emit.test loc loc
                freeNotAlive
                saveLiveVals
                Emit.je l1'
                Emit.jmp l2'
            IJmp _ li -> do
                freeNotAlive
                saveLiveVals
                li' <- label li
                Emit.jmp li'
            IPhi {} -> error "internal error. phi should be eliminated before assembly codegen"
            _ -> error $ "unimplemented " ++ show instr
        fullTrace
        return ()
    )

genCall :: Call a -> GenM ()
genCall call = case call of
    Call _ _ qi args -> do
        let argRegs = zip args (map argReg [0..])
            callIdent = getCallTarget qi
        callerSavedRegs <- gets (filter (\r -> regType r == CallerSaved) . Map.keys . regs)
        forM_ argRegs (uncurry passArg)
        forM_ callerSavedRegs reserveReg
        forM_ callerSavedRegs freeReg
        forM_ callerSavedRegs unreserveReg
        Emit.call callIdent
    CallVirt {}    -> error "callvirt not implemented"
    where passArg val mbreg = case mbreg of
                        Just reg_ -> moveToReg val reg_
                        Nothing   -> error "calls over 6 args not implemented"

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
    vis <- gets (Map.keys . vals)
    let freeIfNotAlive vi = unless (Set.member (toStr vi) (Map.keysSet (liveOut live))) (free vi)
    forM_ vis freeIfNotAlive

free :: ValIdent -> GenM ()
free vi = do
    mbvalS <- lookupValS vi
    case mbvalS of
        Just valS -> do
            forM_ (valLocs valS) (freeFromLoc valS)
            valsMap <- gets vals
            let valsMap' = Map.map (\vs -> vs {valAliases = filter (/= vi) (valAliases vs)}) $
                    Map.delete vi valsMap
            modify (\st -> st {vals = valsMap'})
        Nothing -> return ()

freeFromLoc :: ValState -> Loc -> GenM ()
freeFromLoc valS loc = case loc of
    LocReg reg_ -> do
        regS <- getRegS reg_
        let regS' = regS {regVals = filter (/= valName valS) (regVals regS)}
        setRegS regS'
    LocStack n -> do
        s <- gets stack
        let s' = stackRemove n (valSize valS) s
        modify (\st -> st {stack = s'})
    _ -> return ()

moveToReg :: Val a -> Reg -> GenM ()
moveToReg val reg_ = do
    freeReg reg_
    loc <- getValLoc val
    size <- getValSize val
    comment <- case val of
        VVal _ _ vi -> saveInReg vi reg_ >> return ("moving " ++ toStr vi)
        _           -> return ""
    Emit.movToReg size loc reg_ comment

moveToAnyReg :: Val a -> GenM Reg
moveToAnyReg val = do
    mbreg <- getValUnreservedReg val
    case mbreg of
        Just reg_ -> return reg_
        Nothing   -> do
            reg_ <- chooseReg
            moveToReg val reg_
            return reg_
        where chooseReg = do
                regSs <- gets (filter (not . regReserved) . Map.elems . regs)
                ranks <- mapM rankReg regSs
                return $ reg $ fst $ minimumBy (comparing snd) (zip regSs ranks)
              rankReg regS = if null $ regVals regS then return $ Free (regType $ reg regS)
                             else do
                                live <- asks liveness
                                let nextUse = minimum $ map (\vi -> liveOut live Map.! toStr vi) (regVals regS)
                                valSs <- mapM getValS (regVals regS)
                                return $ if any (all isStack . valLocs) valSs
                                    then Dirty nextUse
                                    else Clean nextUse

isLiveAfter :: ValIdent -> Liveness -> Bool
isLiveAfter vi live = Map.member (toStr vi) (liveOut live)

getValLoc :: Val a -> GenM Loc
getValLoc val = case val of
    VInt _ n    -> return $ LocImm (fromInteger n)
    VTrue _     -> return $ LocImm 1
    VFalse _    -> return $ LocImm 0
    VNegInt _ n -> return $ LocImm (-(fromInteger n))
    VVal _ _ vi -> do
        valS <- getValS vi
        return $ case sort $ valLocs valS of
            []  -> error $ "no locations for val " ++ toStr vi
            x:_ -> x
    _ -> error "lalala"

getValReg :: Val a -> GenM (Maybe Reg)
getValReg val = case val of
    VVal _ _ vi -> do
        valS <- getValS vi
        return $ asReg <$> find isReg (valLocs valS)
    _ -> return Nothing

getValUnreservedReg :: Val a -> GenM (Maybe Reg)
getValUnreservedReg val = case val of
    VVal _ _ vi -> do
        valS <- getValS vi
        let regLocs = filter isReg (valLocs valS)
        regSs <- mapM (getRegS . asReg) regLocs
        return $ reg <$> find (not . regReserved) regSs
    _ -> return Nothing

reserveReg :: Reg -> GenM ()
reserveReg reg_ = do
    regS <- getRegS reg_
    let regS' = regS {regReserved = True}
    setRegS regS'

unreserveReg :: Reg -> GenM ()
unreserveReg reg_ = do
    regS <- getRegS reg_
    let regS' = regS {regReserved = False}
    setRegS regS'

freeReg :: Reg -> GenM ()
freeReg reg_ = do
    regS <- getRegS reg_
    reserveReg reg_
    forM_ (regVals regS) go
    setRegS (regS {regVals = []}) -- sets reserve state back
    where
        go vi = do
            secureValue vi
            valS <- getValS vi
            let valS' = valS {valLocs = filter (\r -> r /= LocReg reg_) (valLocs valS)}
            setValS valS'

secureValue :: ValIdent -> GenM ()
secureValue vi = do
    live <- asks liveness
    valS <- getValS vi
    when (isLiveAfter vi live && all isReg (valLocs valS)) (void $ moveToAnyReg (VVal () undefined vi))

labelFor :: QIdent a -> LabIdent -> LabIdent
labelFor (QIdent _ (SymIdent i1) (SymIdent i2)) (LabIdent l1) = LabIdent $ i1 ++ "." ++ i2 ++ l1

saveInReg :: ValIdent -> Reg -> GenM ()
saveInReg vi reg_ = do
    regS <- useReg reg_
    freeReg reg_
    valS <- getValS vi
    when (regReserved regS) (error $ "internal error. attempt to use reserved register " ++ regHigh reg_)
    let valS' = valS {valLocs = LocReg reg_ : valLocs valS}
        regS' = regS {regVals = valAliases valS}
    setValS valS'
    setRegS regS'

newVal :: ValIdent -> Size -> GenM ()
newVal vi size = do
    free vi
    setValS $ ValS vi size [] [vi]

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

getValS :: ValIdent -> GenM ValState
getValS vi = do
    mb <- gets ((Map.lookup vi) . vals)
    case mb of
        Nothing -> error $ "lalalalalala2" ++ show vi
        Just g  -> return g

lookupValS :: ValIdent -> GenM (Maybe ValState)
lookupValS vi = gets (Map.lookup vi . vals)

getRegS :: Reg -> GenM RegState
getRegS reg_ = do
    mb <- gets ((Map.lookup reg_) . regs)
    case mb of
        Nothing -> error $ "dupalalalalala3" ++ show (regHigh reg_)
        Just g  -> return g

setValS :: ValState -> GenM ()
setValS valS = modify (\st -> st {vals = Map.insert (valName valS) valS (vals st)})

setRegS :: RegState -> GenM ()
setRegS regS = modify (\st -> st {regs = Map.insert (reg regS) regS (regs st)})

saveLiveVals :: GenM ()
saveLiveVals = do
    vis <- gets (Map.keys . vals)
    mapM_ saveOnStack vis
    rs <- gets (Map.keys . regs)
    mapM_ freeReg rs

saveOnStack :: ValIdent -> GenM ()
saveOnStack vi = do
    valS <- getValS vi
    s <- gets stack
    let (s', loc) = stackPush (valSize valS) s
        mbsrcLoc = nonStackLoc valS
    case mbsrcLoc of
        Nothing     -> return ()
        Just srcLoc -> do
            let valS' = valS {valLocs = LocStack loc : valLocs valS}
            Emit.movToStack (valSize valS) srcLoc loc ("save " ++ toStr vi ++ "")
            setValS valS'
            modify (\st -> st {stack = s'})

nonStackLoc :: ValState -> Maybe Loc
nonStackLoc valS = find isNonStack (valLocs valS)

getValSize :: Val a -> GenM Size
getValSize val = case val of
    VInt _  _   -> return Double
    VTrue _     -> return Byte
    VFalse _    -> return Byte
    VNull _     -> return Quadruple
    VVal _ _ vi -> getValS vi >>= (return . valSize)
    _           -> error "lalala"

label :: LabIdent -> GenM LabIdent
label l = asks (`labelGen` l)

endBlock :: GenM ()
endBlock = modify (\st -> st {allCode = bbCode st ++ allCode st, bbCode = []})

newConst :: String -> GenM Const
newConst s = do
    n <- gets constIdx
    modify (\st -> st {constIdx = n + 1})
    return $ Const ("__const_" ++ show n) s

-- Debug

fullTrace :: GenM ()
fullTrace = do
    idx <- gets constIdx
    modify (\st -> st{constIdx = idx + 1})
    traceM ("tick " ++ show idx)
    live <- asks liveness
    traceM ("live: " ++ show (Set.elems $ Map.keysSet $ liveOut live))
    valSs <- gets (Map.elems . vals)
    mapM_ (\vs -> traceM  ("value " ++ toStr (valName vs) ++ ", "
            ++ "aliases: " ++ intercalate ", " (map toStr (valAliases vs))
            ++ " locs: " ++ intercalate ", " (map show (valLocs vs)))) valSs
