module Espresso.CodeGen.Generator (generateEspresso) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Espresso.Syntax.Abs       as Espresso
import           Identifiers
import           SemanticAnalysis.Analyser (SemData (..), Symbol (..),
                                            SymbolTable (..), symTabLookup)
import           SemanticAnalysis.Class    as Class
import           SemanticAnalysis.TopLevel as TopLevel
import qualified Syntax.Abs                as Latte
import           Syntax.Code
import           Utilities                 (dedupBy, single)

data Store = St {
    stLabelCnt :: Integer,
    stValCnt   :: Integer,
    stSymCnt   :: Map.Map Latte.Ident Integer,
    stCode     :: [Instr ()]
}
newtype Env = Env {
    envSymbols :: Map.Map Latte.Ident LatVal
}

data LatVal = LatVal {valName :: ValIdent, _valType :: SType ()}

type GenM = StateT Store (Reader Env)

freshLabel :: GenM LabIdent
freshLabel = labIdent . show <$> freshLabelIdx

idxLabel :: String -> Integer -> LabIdent
idxLabel s n = labIdent (s ++ show n)

freshLabelIdx :: GenM Integer
freshLabelIdx = do
    n <- gets stLabelCnt
    modify (\s -> s {stLabelCnt = n + 1})
    return n

mbAnnLabel :: LabIdent -> Maybe (Int, Int) -> Instr ()
mbAnnLabel l Nothing         = ILabel () l
mbAnnLabel l (Just (lf, lt)) = ILabelAnn () l (toInteger lf) (toInteger lt)

freshVal :: GenM ValIdent
freshVal = do
    n <- gets stValCnt
    modify (\s -> s {stValCnt = n + 1})
    return $ valIdent $ show n

localSyms :: [(Latte.Ident, LatVal)] -> GenM a -> GenM a
localSyms syms = local (\e -> e {envSymbols = Map.union (Map.fromList syms) (envSymbols e)})

cntSym :: Latte.Ident -> GenM Integer
cntSym i = do
    mbidx <- gets (Map.lookup i . stSymCnt)
    case mbidx of
        Just n -> do
            modify (\s -> s { stSymCnt = Map.insert i (n + 1) (stSymCnt s)})
            return $ n + 1
        Nothing -> do
            modify (\s -> s { stSymCnt = Map.insert i 0 (stSymCnt s)})
            return 0

askSym :: Latte.Ident -> GenM LatVal
askSym i = do
    mbval <- asks (Map.lookup i . envSymbols)
    case mbval of
        Just val -> return val
        Nothing  -> error $ "symbol not found: " ++ Latte.showI i

emit :: Instr () -> GenM ()
emit instr = modify (\s -> s {stCode = instr : stCode s})

generateEspresso :: TopLevel.Metadata SemData -> Program ()
generateEspresso (TopLevel.Meta meta) =
    let cls = Map.elems meta
        preamble = Espresso.Meta () $ map genClDef cls
        code = map genMthd $ dedupBy mthdQIdent $ concatMap clMethods cls
    in  Program () preamble code

genClDef :: Class.Class SemData -> Espresso.ClassDef ()
genClDef cl = ClDef () (toSymIdent $ clName cl)
                (map emitFldDef $ clFields cl)
                (map emitMthdDef $ clMethods cl)
    where
        emitFldDef fld = FldDef () (toSType $ fldType fld) (toSymIdent $ fldName fld)
        emitMthdDef mthd = MthdDef () (toFType $ mthdType mthd) (mthdQIdent mthd)

genMthd :: Class.Method SemData -> Espresso.Method ()
genMthd mthd =
    let code = runIdentity $ runReaderT (evalStateT go $ St 0 0 Map.empty []) (Env Map.empty)
        params = map (\(Latte.Arg _ t (Latte.Ident i)) ->
            Param () (toSType $ () <$ t) (ValIdent $ "%a_" ++ i)) (mthdArgs mthd)
    in Espresso.Mthd () (toSType $ () <$ mthdRet mthd) (mthdQIdent mthd) params code
    where
        go = do
            let Latte.Block _ stmts = mthdBlk mthd
                decls = declArgs $ mthdArgs mthd
            emit $ mbAnnLabel entryLabel (codeLines (mthdBlk mthd))
            localSyms decls (genStmts stmts)
            code <- gets (reverse . stCode)
            if mthdRet mthd == Latte.Void () then return $ unifyVRet code else unifyRet (mthdRet mthd) code

genStmts :: [Latte.Stmt SemData] -> GenM ()
genStmts [] = return ()
genStmts (stmt : stmts) = case stmt of
    Latte.Empty _     -> return ()
    Latte.BStmt _ (Latte.Block _ stmts') -> do
        env <- ask
        genStmts stmts'
        local (const env) (genStmts stmts)
    Latte.Decl _ t items -> do
        let syms = semSymbols $ Latte.unwrap stmt
        decls <- genItems t syms items
        localSyms decls (genStmts stmts)
    Latte.Ass _ e1 e2 -> do
        (lvalue, type_) <- genLValue e1
        rvalue <- genExpr e2
        case type_ of
            VLocal    -> emit $ ISet () (valName lvalue) rvalue
            VIndirect -> emit $ IStore () rvalue (toVVal lvalue)
        genStmts stmts
    Latte.Incr _ i    -> do
        val <- askSym i
        emit $ IOp () (valName val) (toVVal val) (OpAdd ()) (VInt () 1)
        genStmts stmts
    Latte.Decr _ i    -> do
        val <- askSym i
        emit $ IOp () (valName val) (toVVal val) (OpSub ()) (VInt () 1)
        genStmts stmts
    Latte.Ret _ e     -> do
        val <- genExpr e
        emit $ IRet () val
        genStmts stmts
    Latte.VRet _      -> do
        emit $ IVRet ()
        genStmts stmts
    Latte.Cond _ cond stmtTrue -> do
        l <- freshLabelIdx
        let lthen = idxLabel "then" l
            lelse = idxLabel "else" l
        genCond cond lthen lelse
        emit $ mbAnnLabel lthen (codeLines stmtTrue)
        genStmts [stmtTrue]
        emit $ IJmp () lelse -- Redundant jump for easier CFG generation.
        emit $ ILabel () lelse
        genStmts stmts
    Latte.CondElse _ cond stmtTrue stmtFalse -> do
        l <- freshLabelIdx
        let lthen = idxLabel "then" l
            lelse = idxLabel "else" l
            lafter = idxLabel "after" l
        genCond cond lthen lelse
        emit $ mbAnnLabel lthen (codeLines stmtTrue)
        genStmts [stmtTrue]
        emit $ IJmp () lafter
        emit $ mbAnnLabel lelse (codeLines stmtFalse)
        genStmts [stmtFalse]
        emit $ IJmp () lafter  -- Redundant jump for easier CFG generation.
        emit $ ILabel () lafter
        genStmts stmts
    Latte.While _ cond body -> do
        l <- freshLabelIdx
        let lcond = idxLabel "cond" l
            lbody = idxLabel "body" l
            lafter = idxLabel "after" l
        emit $ IJmp () lcond
        emit $ mbAnnLabel lbody (codeLines body)
        genStmts [body]
        emit $ IJmp () lcond  -- Redundant jump for easier CFG generation.
        emit $ mbAnnLabel lcond (codeLines cond)
        genCond cond lbody lafter
        emit $ ILabel () lafter
        genStmts stmts
    Latte.For {} -> error "for should be rewritten"
    Latte.SExp _ e          -> do
        _ <- genExpr e
        genStmts stmts

genItems :: Latte.Type SemData -> SymbolTable -> [Latte.Item SemData] -> GenM [(Latte.Ident, LatVal)]
genItems t syms = mapM genItem
    where genItem item = do
            (i, val) <- genVal item
            idx <- cntSym i
            let suf = if idx == 0 then "" else '_':show idx
                newval = valIdent $ Latte.showI i ++ suf
            emit $ ISet () newval val
            return (i, LatVal newval (toSType $ () <$ t))
          genVal item = case item of
            Latte.NoInit _ i -> do
                let sym = fromJust $ symTabLookup i syms
                    val = defaultVal (toSType $ symType sym)
                return (i, val)
            Latte.Init _ i e -> do
                val <- genExpr e
                return (i, val)

declArgs :: [Latte.Arg Code] -> [(Latte.Ident, LatVal)]
declArgs = map declArg
    where
        declArg :: Latte.Arg Code -> (Latte.Ident, LatVal)
        declArg (Latte.Arg _ t i) = (i, LatVal (argValIdent $ toStr i) (toSType $ () <$ t))

genExpr :: Latte.Expr SemData -> GenM (Val ())
genExpr expr = case expr of
    Latte.EVar _ i        -> do
        val <- askSym i
        return (toVVal val)
    Latte.ELitInt _ n     -> return $ VInt () n
    Latte.EString _ s     -> return $ VStr () s
    Latte.ELitTrue _      -> return $ VTrue ()
    Latte.ELitFalse _     -> return $ VFalse ()
    Latte.ENullI _ _      -> error "should be converted to ENull"
    Latte.ENullArr _ _    -> error "should be converted to ENullArr"
    Latte.ENull _ _       -> return $ VNull ()
    Latte.ENew _ _        -> error "objects unimplemented"
    Latte.ENewArr _ _ _   -> error "arrays unimplemented"
    Latte.EApp sem e args   -> do
        let t = semType sem
        vals <- mapM genExpr args
        fun <- genFun e
        newval <- freshVal
        emit $ ICall () newval (Call () (toSType t) fun vals)
        return (VVal () (toSType t) newval)
    Latte.EIdx {}         -> error "arrays unimplemented"
    Latte.EAcc {}         -> error "objects unimplemented"
    Latte.ENeg _ e        -> genUnOp e (UnOpNeg ())
    Latte.ENot _ e        -> genUnOp e (UnOpNot ())
    Latte.EMul _ e1 op e2 -> genOp e1 e2 (toEspressoMulOp op)
    Latte.EAdd _ e1 op e2 -> genOp e1 e2 (toEspressoAddOp op)
    Latte.ERel _ e1 op e2 -> genOp e1 e2 (toEspressoRelOp op)
    Latte.EAnd {}    -> do
        l <- freshLabelIdx
        let ltrue = idxLabel "true" l
            lfalse = idxLabel "false" l
        newval <- freshVal
        emit $ ISet () newval (VFalse ())
        genCond expr ltrue lfalse
        emit $ ILabel () ltrue
        emit $ ISet () newval (VTrue ())
        emit $ IJmp () lfalse -- Redundant jump for easier CFG generation.
        emit $ ILabel () lfalse
        return $ VVal () (Bool ()) newval
    Latte.EOr {}     -> do
        l <- freshLabelIdx
        let ltrue = idxLabel "true" l
            lfalse = idxLabel "false" l
        newval <- freshVal
        emit $ ISet () newval (VTrue ())
        genCond expr ltrue lfalse
        emit $ ILabel () lfalse
        emit $ ISet () newval (VFalse ())
        emit $ IJmp () ltrue -- Redundant jump for easier CFG generation.
        emit $ ILabel () ltrue
        return $ VVal () (Bool ()) newval

genCond :: Latte.Expr SemData -> LabIdent -> LabIdent -> GenM ()
genCond e ltrue lfalse = case e of
    Latte.EAnd _ e1 e2 -> do
        lmid <- freshLabel
        genCond e1 lmid lfalse
        emit $ ILabel () lmid
        genCond e2 ltrue lfalse
    Latte.EOr _ e1 e2 -> do
        lmid <- freshLabel
        genCond e1 ltrue lmid
        emit $ ILabel () lmid
        genCond e2 ltrue lfalse
    _ -> do
        val <- genExpr e
        emit $ ICondJmp () val ltrue lfalse

genUnOp :: Latte.Expr SemData -> UnOp () -> GenM (Val ())
genUnOp e op = do
    let t = semType $ single e
    val <- genExpr e
    newval <- freshVal
    emit $ IUnOp () newval op val
    return $ VVal () (toSType t) newval

genOp :: Latte.Expr SemData -> Latte.Expr SemData -> Op () -> GenM (Val ())
genOp e1 e2 op = do
    let t = semType $ single e1
    val1 <- genExpr e1
    val2 <- genExpr e2
    newval <- freshVal
    emit $ IOp () newval val1 op val2
    return $ VVal () (toSType t) newval

unifyVRet :: [Instr ()] -> [Instr ()]
unifyVRet instr = let instr' = go instr []
                  in  reverse instr' ++ [ILabel () exitLabel, IVRet ()]
    where
        go [] x            = IJmp () exitLabel:x
        go (IVRet ():is) x = go is (IJmp () exitLabel:x)
        go (i:is) x        = go is (i:x)

unifyRet :: Latte.Type () -> [Instr ()] -> GenM [Instr ()]
unifyRet t instr = do
    (instr', phi) <- go instr [] [] entryLabel
    idx <- cntSym (Latte.Ident "ret")
    let val = valIdent $ "ret" ++ if idx == 0 then "" else '_':show idx
    return $ reverse instr' ++ [ILabel () exitLabel, IPhi () val phi, IRet () (VVal () (toSType t) val)]
    where
        go [] x phi _                  = return (x, phi)
        go (i@(ILabel _ l):is) x phi _ = go is (i:x) phi l
        go (i@(ILabelAnn _ l _ _):is) x phi _ = go is (i:x) phi l
        go (IRet _ v:is) x phi l       = go is (IJmp () exitLabel:x) (PhiVar () l v:phi) l
        go (i:is) x phi l              = go is (i:x) phi l

toEspressoMulOp :: Latte.MulOp SemData -> Op ()
toEspressoMulOp op = case op of
    Latte.Times _ -> OpMul ()
    Latte.Div _   -> OpDiv ()
    Latte.Mod _   -> OpMod ()

toEspressoAddOp :: Latte.AddOp SemData -> Op ()
toEspressoAddOp op = case op of
    Latte.Plus _  -> OpAdd ()
    Latte.Minus _ -> OpSub ()

toEspressoRelOp :: Latte.RelOp SemData -> Op ()
toEspressoRelOp op = case op of
    Latte.EQU _ -> OpEQU ()
    Latte.NE _  -> OpNE ()
    Latte.GE _  -> OpGE ()
    Latte.GTH _ -> OpGTH ()
    Latte.LE _  -> OpLE ()
    Latte.LTH _ -> OpLTH ()

data ValType = VLocal | VIndirect

genLValue :: Latte.Expr SemData -> GenM (LatVal, ValType)
genLValue e = case e of
    Latte.EVar _ i -> do
        val <- askSym i
        return (val, VLocal)
    Latte.EIdx {} -> error "arrays unimplemented"
    Latte.EAcc {} -> error "objects unimplemented"
    _ -> error $ "internal error, invalid lvalue " ++ show (() <$ e)

genFun :: Latte.Expr SemData -> GenM (QIdent ())
genFun e = case e of
    Latte.EVar _ i    -> return $ QIdent () (SymIdent $ toStr topLevelClassIdent) (SymIdent $ toStr i)
    Latte.EAcc {} -> error "objects unimplemented"
    _ -> error $ "internal error, invalid function " ++ show (() <$ e)

defaultVal :: SType a -> Val ()
defaultVal t = case deref t of
    Int _   -> VInt () 0
    Str _   -> VStr () []
    Bool _  -> VFalse ()
    Cl _ _  -> VNull ()
    Arr _ _ -> VNull ()
    _       -> error $ "invalid type " ++ show (() <$ t)

deref :: SType a -> SType a
deref t = case t of
    Ref _ t' -> t'
    _        -> t

mthdQIdent :: Class.Method a -> QIdent ()
mthdQIdent mthd =
    let mName = (toSymIdent $ mthdName mthd)
    in case mthdSelf mthd of
        Just t  -> QIdent () (SymIdent $ showType t) mName
        Nothing -> QIdent () (toSymIdent topLevelClassIdent) mName

toSymIdent :: Latte.Ident -> SymIdent
toSymIdent (Latte.Ident s) = SymIdent s

codeLines :: (Functor f, Foldable f) => f SemData -> Maybe (Int, Int)
codeLines stmt = do
    lMin <- foldr (lift_ min . semToLine) Nothing stmt
    lMax <- foldr (lift_ max . semToLine) Nothing stmt
    return (lMin, lMax)
    where semToLine s = fst <$> (semCode s >>= codePos)
          lift_ _ Nothing x         = x
          lift_ _ x Nothing         = x
          lift_ f (Just x) (Just y) = Just $ f x y

toSType :: Latte.Type a -> SType a
toSType t = case t of
    Latte.Int a                -> Int a
    Latte.Str a                -> Ref a (Str a)
    Latte.Bool a               -> Bool a
    Latte.Void a               -> Void a
    Latte.Var {}               -> error "not a simple type 'var'"
    Latte.Arr a t'             -> Arr a (toSType t')
    Latte.Cl a i               -> Cl a (toSymIdent i)
    Latte.Fun{}                -> error "not a simple type Fun"
    Latte.Ref _ (Latte.Int a)  -> Int a
    Latte.Ref _ (Latte.Bool a) -> Bool a
    Latte.Ref a t'             -> Ref a (deref $ toSType t')

toFType :: Latte.Type a -> FType a
toFType t = case t of
    Latte.Fun a r ps -> FType a (toSType r) (map toSType ps)
    _                -> error "not a function type"

toVVal :: LatVal -> Val ()
toVVal (LatVal i t) = VVal () t i
