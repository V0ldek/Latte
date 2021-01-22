{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Espresso.Interpreter (interpret) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor       (Bifunctor (first))
import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe
import           Espresso.Syntax.Abs
import qualified Espresso.Types       as Types
import           Identifiers
import           LatteIO
import           System.Exit          (ExitCode (..))

type Loc = Int
type OEnv = Map.Map String Loc
type CEnv = Map.Map String Class
type LEnv = Map.Map String [Instr Pos]
type PEnv = Map.Map Integer (Param ())
type Store = Map.Map Loc Object

data Env = Env { objEnv :: OEnv, clEnv :: CEnv, labelEnv :: LEnv, paramEnv :: PEnv }

data Object = Inst { objType_ :: Class, objData_ :: OEnv }
            | Array  { arrType_ :: SType (), arrData_ :: [Loc], arrLength_ :: Int}
            | PRef   { ref :: Loc }
            | PInt Int
            | PBool Bool
            | PStr String
            | PNull

data Class = Class { clName :: String, clFlds :: Map.Map String Field, clMthds :: Map.Map String Function }

data Field = Fld { fldName :: String, fldType :: SType () }
data Function = Fun { funName :: String, funRet :: SType (), funParams :: [Param ()], funCode :: [Instr Pos] }

type InterpreterM m = StateT Store (ReaderT Env m)

interpret :: (LatteIO m, Monad m) => Program Pos -> m ()
interpret (Program _ (Meta _ clDefs) methods) = do
    obj <- runReaderT (evalStateT go Map.empty) (Env Map.empty env Map.empty Map.empty)
    let PInt n = obj
    if n == 0 then exitSuccess else exitWith $ ExitFailure n
    where
        env = let cls = map clDefToCl clDefs
                    in Map.fromList (zip (map clName cls) cls)
        clDefToCl (ClDef _ i fldDefs mthdDefs) = let flds = map fldDefToFld fldDefs
                                                     mthds = mapMaybe mthdDefToFun mthdDefs
                                                     fldMap = Map.fromList $ zip (map fldName flds) flds
                                                     mthdMap = Map.fromList $ zip (map funName mthds) mthds
                                                 in Class (toStr i) fldMap mthdMap
        fldDefToFld (FldDef _ t i) = Fld (toStr i) (() <$ t)
        mthdDefToFun (MthdDef _ (FType _ r _) qi) =
            let QIdent _ cli i = qi
            in  if isNativeFun (toStr cli) (toStr i)
                    then Nothing
                    else let Mthd _ _ _ ps code = case Map.lookup (cli, i) methodMap of
                                Nothing -> Prelude.error $ "internal error, method " ++ toStr cli ++ "." ++ toStr i ++ " not found"
                                Just m  -> m
                         in  Just $ Fun (toStr i) (() <$ r) (map (() <$) ps) code
        methodMap = Map.fromList $ zip (map (\(Mthd _ _ (QIdent _ cli i) _ _) -> (cli, i)) methods) methods
        go = do
            main <- askFun (toStr topLevelClassIdent) (toStr mainSymIdent)
            call main [] return

call :: (LatteIO m, Monad m) => Function -> [Object] -> (Object -> InterpreterM m a) -> InterpreterM m a
call (Fun _ _ ps code) objs ret = do
    argEnv <- allocs args
    let labels = getLabels code
        params = zip [0..] ps
    ret' <- saveEnv2 ret
    localParams params $ localObjs argEnv $ localLabels labels $ execute (toStr entryLabel) (toStr entryLabel) code ret'
    where
        args = zipWith (\(Param _ _ vi) p -> (toStr vi, p)) ps objs

getLabels :: [Instr Pos] -> [(String, [Instr Pos])]
getLabels [] = []
getLabels (i : is) = case i of
    ILabel _ l        -> (toStr l, is) : getLabels is
    ILabelAnn _ l _ _ -> (toStr l, is) : getLabels is
    _                 -> getLabels is

callFromCallsite :: (LatteIO m, Monad m) => Call Pos -> (Object -> InterpreterM m a) -> InterpreterM m a
callFromCallsite callsite ret = case callsite of
    Call _ _ (QIdent _ i1 i2) vals     -> do
        args <- mapM getVal vals
        if isNativeFun (toStr i1) (toStr i2) then runNative (toStr i2) vals ret else (do
            f <- askFun (toStr i1) (toStr i2)
            call f args ret)
    CallVirt _ _ (QIdent _ _ i2) vals -> do
        args <- mapM getVal vals
        case args of
            a:_ -> do
                obj <- deref a
                case obj of
                    Inst cl _ -> do
                        f <- askFun (clName cl) (toStr i2)
                        call f args ret
                    _ -> LatteIO.error $ "internal error. invalid object as self in callvirt " ++ show obj
            []   -> LatteIO.error "internal error. empty arg list in callvirt."

execute :: (LatteIO m, Monad m) => String -> String -> [Instr Pos] -> (Object -> InterpreterM m a) -> InterpreterM m a
execute _ _ [] ret = ret PNull
execute prevLabel currLabel (instr : is) ret = case instr of
    ILabel _ i   -> execute currLabel (toStr i) is ret
    ILabelAnn _ i _ _ -> execute currLabel (toStr i) is ret
    IVRet {}     -> ret PNull
    IRet _ v     -> do
        x <- getVal v
        ret x
    IOp _ i v1 op v2 -> do
        x1 <- getVal v1
        x2 <- getVal v2
        let res = performOp x1 x2 op
        newval <- store (toStr i) res
        localObj newval $ execute prevLabel currLabel is ret
    ISet _ i v -> do
        x <- getVal v
        newval <- store (toStr i) x
        localObj newval $ execute prevLabel currLabel is ret
    ISwap _ _ (ValIdent vi1) (ValIdent vi2) -> do
        mbloc1 <- asks (Map.lookup vi1 . objEnv)
        mbloc2 <- asks (Map.lookup vi2 . objEnv)
        x1 <- case mbloc1 of
            Just loc1 -> deref $ PRef loc1
            Nothing   -> return PNull
        x2 <- case mbloc2 of
            Just loc1 -> deref $ PRef loc1
            Nothing   -> return PNull
        newval1 <- store vi1 x2
        newval2 <- store vi2 x1
        localObj newval1 $ localObj newval2 $ execute prevLabel currLabel is ret
    IUnOp _ i op v -> do
        x <- getVal v
        let res = performUnOp x op
        newval <- store (toStr i) res
        localObj newval $ execute prevLabel currLabel is ret
    IVCall _ callsite -> callFromCallsite callsite (\_ -> execute prevLabel currLabel is ret)
    ICall _ i callsite -> callFromCallsite callsite (\res -> do
        newval <- store (toStr i) res
        localObj newval $ execute prevLabel currLabel is ret)
    INew _ i t -> case Types.deref t of
        Cl _ (SymIdent cli) -> do
            cl <- askCl cli
            locs <- mapM (\f -> allocDefault (fldType f) >>= (\l -> return (f, l))) (Map.elems $ clFlds cl)
            let flds = Map.fromList $ map (first fldName) locs
                obj = Inst cl flds
            loc <- newloc
            storeObj loc obj
            let ref_ = PRef loc
            newval <- store (toStr i) ref_
            localObj newval $ execute prevLabel currLabel is ret
        _ -> Prelude.error $ "internal error. new on a nonclass" ++ show t
    INewArr _ i t val -> do
        size <- getVal val
        case size of
            PInt n -> do
                locs <- replicateM n (allocDefault t)
                let arr = Array (() <$ t) locs n
                newval <- store (toStr i) arr
                localObj newval $ execute prevLabel currLabel is ret
            _ -> Prelude.error $ "internal error. invalid type of size of array " ++ show size
    INewStr _ i str -> do
        newval <- store (toStr i) (PStr str)
        localObj newval $ execute prevLabel currLabel is ret
    IJmp _ i -> do
        is' <- askLabel (toStr i)
        execute currLabel (toStr i) is' ret
    ICondJmp _ v i1 i2 -> do
        x1 <- getVal v
        is1 <- askLabel (toStr i1)
        is2 <- askLabel (toStr i2)
        b <- isTrue x1
        let (label, is') = if b
            then (toStr i1, is1)
            else (toStr i2, is2)
        execute currLabel label is' ret
    ILoad _ i ptr -> do
        x <- loadFrom ptr
        newval <- store (toStr i) x
        localObj newval $ execute prevLabel currLabel is ret
    IStore _ v ptr -> do
        x <- getVal v
        storeInto ptr x
        execute prevLabel currLabel is ret
    IPhi _ i variants -> do
        let Just (PhiVar _ _ val) = find (\(PhiVar _ l _) -> toStr l == prevLabel) variants
        obj <- getVal val
        newval <- store (toStr i) obj
        localObj newval $ execute prevLabel currLabel is ret
    IEndPhi {} -> execute prevLabel currLabel is ret

loadFrom :: (LatteIO m, Monad m) => Ptr a -> InterpreterM m Object
loadFrom ptr = case ptr of
    PArrLen _ v -> do
        x <- getVal v >>= deref
        case x of
            Array _ _ len -> return $ PInt len
            _ -> LatteIO.error $ "internal error. invalid type in arrlen " ++ show x
    PElem _ _ arrV idxV -> do
        arr <- getVal arrV >>= deref
        idx <- getVal idxV
        case (arr, idx) of
            (Array _ data_ len, PInt n) ->
                if len <= n
                  then LatteIO.error $ "internal error. arrptr index overflow " ++ show n ++ " >= " ++ show len
                  else do
                    let loc = data_ !! n
                    gets (Map.! loc)
            _ -> LatteIO.error $ "internal error. invalid types in arrptr " ++ show arr ++ ", " ++ show idx
    PFld _ _ v (QIdent _ _ (SymIdent fldI)) -> do
        x <- getVal v >>= deref
        case x of
            Inst _ flds -> case Map.lookup fldI flds of
                Just loc -> gets (Map.! loc)
                Nothing  -> LatteIO.error $ "internal error. nonexistent field " ++ fldI
            _ -> LatteIO.error $ "internal error. fldptr called on noninstance " ++ show x
    PLocal _ _ n -> do
        mbloc <- gets (Map.lookup (localLoc n))
        case mbloc of
            Just obj -> return obj
            Nothing -> LatteIO.error $ "internal error. local " ++ show n ++ " load before store"
    PParam _ _ n _ -> do
        (Param _ _ (ValIdent vi)) <- askParam n
        loc <- askObj vi
        gets (Map.! loc)

deref :: (LatteIO m, Monad m) => Object -> InterpreterM m Object
deref x = case x of
    PRef loc -> do
        mbobj <- gets (Map.lookup loc)
        case mbobj of
            Nothing  -> LatteIO.error ("internal error, pointer " ++ show loc ++ " not found")
            Just obj -> return obj
    _       -> return x

askFun :: (LatteIO m, Monad m) => String -> String -> InterpreterM m Function
askFun clI mthdI = do
    cl <- askCl clI
    case Map.lookup mthdI (clMthds cl) of
        Nothing -> LatteIO.error ("internal error, method " ++ clI ++ "." ++ mthdI ++ " not found")
        Just mthd -> return mthd

askCl :: (LatteIO m, Monad m) =>  String -> InterpreterM m Class
askCl clI = do
    mbcl <- asks (Map.lookup clI . clEnv)
    case mbcl of
        Nothing -> LatteIO.error ("internal error, class " ++ clI ++ " not found")
        Just cl -> return cl

askLabel :: (LatteIO m, Monad m) => String -> InterpreterM m [Instr Pos]
askLabel label = do
    mbl <- asks (Map.lookup label . labelEnv)
    case mbl of
        Nothing -> LatteIO.error ("internal error, label " ++ label ++ " not found")
        Just l -> return l

askParam :: (LatteIO m, Monad m) => Integer -> InterpreterM m (Param ())
askParam n = do
    mbp <- asks (Map.lookup n . paramEnv)
    case mbp of
        Nothing -> LatteIO.error ("internal error, param " ++ show n ++ " not found")
        Just p -> return p

askObj :: (LatteIO m, Monad m) => String -> InterpreterM m Loc
askObj i = do
    mbobj <- asks (Map.lookup i . objEnv)
    case mbobj of
        Nothing -> LatteIO.error ("internal error, object " ++ i ++ " not found")
        Just l -> return l

allocs :: (LatteIO m, Monad m) => [(String, Object)] -> InterpreterM m [(String, Loc)]
allocs = mapM (uncurry alloc)

alloc :: (LatteIO m, Monad m) => String -> Object -> InterpreterM m (String, Loc)
alloc i obj = do
    loc <- newloc
    storeObj loc obj
    return (i, loc)

store :: (LatteIO m, Monad m) => String -> Object -> InterpreterM m (String, Loc)
store i obj = do
    mbloc <- asks (Map.lookup i . objEnv)
    loc <- maybe newloc return mbloc
    storeObj loc obj
    return (i, loc)

storeObj :: (LatteIO m, Monad m) => Loc -> Object -> InterpreterM m ()
storeObj loc obj = modify $ Map.insert loc obj

storeInto :: (LatteIO m, Monad m) => Ptr Pos -> Object -> InterpreterM m ()
storeInto to obj = case to of
    PArrLen _ v -> LatteIO.error $ "internal error. store to array length " ++ show v
    PElem _ _ arrV idxV -> do
        arr <- getVal arrV
        idx <- getVal idxV
        case (arr, idx) of
            (Array _ data_ len, PInt n) ->
                if len <= n
                  then LatteIO.error $ "internal error. arrptr index overflow " ++ show n ++ " >= " ++ show len
                  else do
                    let loc = data_ !! n
                    storeObj loc obj
            _ -> LatteIO.error $ "internal error. invalid types in arrptr " ++ show arr ++ ", " ++ show idx
    PFld _ _ v (QIdent _ _ (SymIdent fldI)) -> do
        x <- getVal v >>= deref
        case x of
            Inst _ flds -> case Map.lookup fldI flds of
                Just loc -> storeObj loc obj
                Nothing  -> LatteIO.error $ "internal error. nonexistent field " ++ fldI
            _ -> LatteIO.error $ "internal error. fldptr called on noninstance " ++ show x
    PLocal _ _ n -> storeObj (localLoc n) obj
    PParam _ _ n _ -> do
        (Param _ _ (ValIdent vi)) <- askParam n
        loc <- askObj vi
        storeObj loc obj

getVal :: (LatteIO m, Monad m) => Val a -> InterpreterM m Object
getVal val = case val of
    VInt _ n    -> return $ PInt (fromInteger n)
    VNegInt _ n -> return $ PInt (fromInteger (-n))
    VTrue _     -> return $ PBool True
    VFalse _    -> return $ PBool False
    VNull {}    -> return PNull
    VVal _ _ i  -> askObj (toStr i) >>= (deref . PRef)

newloc :: (LatteIO m, Monad m) => InterpreterM m Int
newloc = do
    objs <- get
    if Map.null objs then return 0
    else let (k, _) = Map.findMax objs in return $ k + 1

allocDefault :: (LatteIO m, Monad m, Show a) => SType a -> InterpreterM m Loc
allocDefault t = do
    loc <- newloc
    let val = defaultValue t
    storeObj loc val
    return loc

defaultValue :: Show a => SType a -> Object
defaultValue t = case t of
    Int _  -> PInt 0
    Bool _ -> PBool False
    Ref {} -> PNull
    _ -> Prelude.error $ "internal error. no default value for type " ++ show t

localObjs :: (LatteIO m, Monad m) => [(String, Loc)] -> InterpreterM m a -> InterpreterM m a
localObjs objs = local (\e -> e { objEnv = Map.fromList objs })

localParams :: (LatteIO m, Monad m) => [(Integer, Param ())] -> InterpreterM m a -> InterpreterM m a
localParams ps = local (\e -> e { paramEnv = Map.fromList ps })

localObj :: (LatteIO m, Monad m) => (String, Loc) -> InterpreterM m a -> InterpreterM m a
localObj (i, ptr) = local (\e -> e { objEnv = Map.insert i ptr $ objEnv e })

localLabels :: (LatteIO m, Monad m) => [(String, [Instr Pos])] -> InterpreterM m a -> InterpreterM m a
localLabels labels = local (\e -> e { labelEnv = Map.union (Map.fromList labels) $ labelEnv e })

saveEnv2 :: (LatteIO m, Monad m) => (a -> InterpreterM m b) -> InterpreterM m (a -> InterpreterM m b)
saveEnv2 m = do
    env <- ask
    return (local (const env) . m)

isTrue :: (LatteIO m, Monad m) => Object -> InterpreterM m Bool
isTrue x = case x of
    PBool True -> return True
    _          -> return False

performOp :: Object -> Object -> Op Pos -> Object
performOp x1 x2 op = case (x1, x2, op) of
    (PInt n1, PInt n2, OpAdd {}) -> PInt $ n1 + n2
    (PStr s1, PStr s2, OpAdd {}) -> PStr $ s1 ++ s2
    (PInt n1, PInt n2, OpSub {}) -> PInt $ n1 - n2
    (PInt n1, PInt n2, OpMul {}) -> PInt $ n1 * n2
    (PInt n1, PInt n2, OpDiv {}) -> PInt $ n1 `div` n2
    (PInt n1, PInt n2, OpMod {}) -> PInt $ n1 `mod` n2
    (_, _, OpEQU {})             -> PBool $ areEq x1 x2
    (_, _, OpNE {})              -> PBool $ not $ areEq x1 x2
    (_, _, OpGE {})              -> PBool $ getOrd x1 x2 op
    (_, _, OpGTH {})             -> PBool $ getOrd x1 x2 op
    (_, _, OpLE {})              -> PBool $ getOrd x1 x2 op
    (_, _, OpLTH {})             -> PBool $ getOrd x1 x2 op
    _                            -> Prelude.error (show op)

performUnOp :: Object -> UnOp Pos -> Object
performUnOp x op = case (x, op) of
    (PInt n, UnOpNeg {})  -> PInt $ -n
    (PBool b, UnOpNot {}) -> PBool $ not b
    _                     -> Prelude.error (show op)

areEq :: Object -> Object -> Bool
areEq x1 x2 = case (x1, x2) of
    (PInt n1, PInt n2)   -> n1 == n2
    (PStr s1, PStr s2)   -> s1 == s2
    (PBool b1, PBool b2) -> b1 == b2
    (PNull, PNull)       -> True
    (PRef p1, PRef p2)   -> p1 == p2
    _                    -> False

getOrd :: Object -> Object -> Op Pos -> Bool
getOrd x1 x2 op = case (x1, x2) of
    (PInt n1, PInt n2)   -> ordWithOp n1 n2
    (PStr s1, PStr s2)   -> ordWithOp s1 s2
    (PBool b1, PBool b2) -> ordWithOp b1 b2
    _                    -> Prelude.error $ "invalid ord of " ++ show x1 ++ ", " ++ show x2
    where
        ordWithOp x1' x2' = case op of
                OpGTH {} -> x1' > x2'
                OpGE {}  -> x1' >= x2'
                OpLTH {} -> x1' < x2'
                OpLE {}  -> x1' <= x2'
                _        -> Prelude.error $ "internal error, invalid relop " ++ show op

localLoc :: Integer -> Loc
localLoc n = fromInteger $ (-n) - 1

isNativeFun :: String -> String -> Bool
isNativeFun i1 i2 = i1 == toStr topLevelClassIdent &&
    case i2 of
        "printInt"    -> True
        "printString" -> True
        "error"       -> True
        "readInt"     -> True
        "readString"  -> True
        _             -> False

runNative :: (LatteIO m, Monad m) => String -> [Val Pos] -> (Object -> InterpreterM m a) -> InterpreterM m a
runNative i vals ret = case i of
    "printInt"    -> do
        x <- getVal (head vals)
        let PInt n = x
        printInt n
        ret PNull
    "printString" -> do
        x <- getVal (head vals)
        let PStr s = x
        printString s
        ret PNull
    "error"       -> LatteIO.error ""
    "readInt"     -> readInt >>= ret . PInt
    "readString"  -> readString >>= ret . PStr
    _             -> LatteIO.error $ "internal error, invalid native fun " ++ i

instance Show Object where
    show o = case o of
        Inst t _    -> clName t
        Array t _ l -> show t ++ "[" ++ show l ++ "]"
        PRef t      -> "&" ++ show t
        PInt _      -> "int"
        PBool _     -> "bool"
        PStr _      -> "string"
        PNull       -> "null"

instance (Monad m, LatteIO m) => LatteIO (InterpreterM m) where
    readInt = lift $ lift readInt
    readString = lift $ lift readString
    error s = lift $ lift $ LatteIO.error s
    printInt n = lift $ lift $ printInt n
    printString s = lift $ lift $ printString s
    printErrorString s = lift $ lift $ printErrorString s
    doesDirectoryExist d = lift $ lift $ doesDirectoryExist d
    doesFileExist f = lift $ lift $ doesFileExist f
    readFile f = lift $ lift $ LatteIO.readFile f
    writeFile f c = lift $ lift $ LatteIO.writeFile f c
    exitWith c = lift $ lift $ exitWith c
    exitSuccess = lift $ lift exitSuccess
    exitFailure = lift $ lift exitFailure
