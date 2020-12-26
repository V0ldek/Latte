{-# LANGUAGE FlexibleInstances #-}
-- Semantic static analysis rejecting incorrect programs and adding type annotations to the program tree.
module SemanticAnalysis.Analyser (analyse, SemData (..), Symbol (..), SymbolTable (..), symTabLookup) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List                    (find, intercalate)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, isJust)
import qualified Error
import           Identifiers
import           SemanticAnalysis.Class
import           SemanticAnalysis.ControlFlow
import           SemanticAnalysis.TopLevel    (Metadata (..))
import           Syntax.Abs
import           Syntax.Code
import           Syntax.Printer               (Print, printTree)
import           Utilities

-- Tree of symbol tables representing the scoped declarations.
data SymbolTable = SymTab {symTab :: Map.Map Ident Symbol, symParent :: Maybe SymbolTable}

-- Result of the analysis phase included in each node.
data SemData = SemData {
  semSymbols   :: SymbolTable, -- Symbol table calculated at given place in the program.
  semType      :: Type (),     -- Type of the expression, Void for statements.
  semCode      :: Maybe Code,  -- Original source code of the given node, might be Nothing for generated code.
  semReachable :: Reachability -- Reachability status at given place in the program.
}

-- A declared symbol with a name and a type.
data Symbol = Sym {symName :: Ident, symType :: Type (), symCode :: Maybe Code}

-- Store of already analysed classes.
type ClassStore = Map.Map Ident (Class SemData)

-- State of the analyser.
data Store = Store {
  stClasses      :: ClassStore,   -- Store of already analysed classes.
  stSymTab       :: SymbolTable,  -- Current symbol table.
  stReachability :: Reachability  -- Current reachability status.
}

-- Analyser monad combining the state and a read-only storage of metadata from the TopLevel analyser.
type AnalyserM = StateT Store (ReaderT (Metadata Code) (Either String))

instance Positioned SemData where
  pos x = semCode x >>= codePos

instance Positioned Symbol where
  pos x = symCode x >>= codePos

instance ControlFlow AnalyserM where
    getReach = gets stReachability
    setReach r = modify (\st -> st { stReachability = r })

instance Error.WithContext SemData where
  getCtx = Error.getCtx . semCode

-- Analyse parsed TopLevel metadata.
analyse :: Metadata Code -> Either String (Metadata SemData)
analyse meta = runReaderT (evalStateT go (Store Map.empty (topLevelSymTab meta) (Reach True))) meta
  where
    Meta m = meta
    clIdents = Map.keys m
    cls = Map.elems m
    go :: AnalyserM (Metadata SemData)
    go = do
      cls' <- mapM analyseCl cls
      return $ Meta (Map.fromList $ zip clIdents cls')

-- Analyse a given class metadata.
analyseCl :: Class Code -> AnalyserM (Class SemData)
analyseCl cl = do
  mbCl <- getsCls $ Map.lookup (clName cl)
  case mbCl of
    Just cl' -> return cl'            -- Memoized result.
    Nothing -> do
      base <- mbAnalyseCl (clBase cl) -- Make sure base is analysed first so we can access
                                      -- its field and method symbols.
      enterCl cl
      mthds' <- mapM analyseMthd (clMethods cl)
      exitCl
      let cl' = cl {clBase = base, clMethods = mthds'}
      storeClass cl'
      return cl'

-- analyseCl lifted with Nothing coalescing.
mbAnalyseCl :: Maybe (Class Code) -> AnalyserM (Maybe (Class SemData))
mbAnalyseCl cl = case cl of
  Nothing  -> return Nothing
  Just cl' -> Just <$> analyseCl cl'

-- Analyse a given method metadata. Assumes that its enclosing class was entered with enterCl.
analyseMthd :: Method Code -> AnalyserM (Method SemData)
analyseMthd m = do
  enterMthd m
  blk' <- analyseBlk (mthdBlk m)
  exitMthd
  return $ m {mthdBlk = blk'}

-- Annalyse a given block. Assumes that a method was already entered and is accessible
-- with getCurrentMethod.
analyseBlk :: Block Code -> AnalyserM (Block SemData)
analyseBlk blk@(Block _ stmts) = do
  enterBlk
  stmts' <- forM stmts analyseStmt
  exitBlk
  semData <- analyseVoid blk
  return $ Block semData stmts'

-- Start analysing a given class, pushing a new symbol table
-- with its fields, methods and the 'self' symbol.
enterCl :: Class a -> AnalyserM ()
enterCl cl =
  let fldSyms = map fldToSym (clFields cl)
      mthdSyms = map mthdToSym (clMethods cl)
      selfSym = Sym selfSymIdent (Cl () (clName cl)) undefined
   in do
        -- Note that methods are checked for type correctness when they are analysed,
        -- so here we only handle the fields.
        mapM_ checkFldType fldSyms
        pushSymTab
        addSyms (fldSyms ++ mthdSyms)
        addSym selfSym
  where
    -- Check if field declarations do not violate typing rules.
    checkFldType sym = do
      let t = symType sym
      when (isVar t) (varFldError (symCode sym))
      when (isVoid t) (voidFldError (symCode sym))
      unlessM (typeExists t) (nonexistentTypeErrorMb t (symCode sym))

-- Cleanup after analysing a class. Since enterCl only creates a symbol table
-- we only need to pop that table.
exitCl :: AnalyserM ()
exitCl = popSymTab

-- Begin analysing a scope block. Amounts to beginning a new empty symbol table.
enterBlk :: AnalyserM ()
enterBlk = pushSymTab

-- Cleanup after analysing a scope block. Amounts to dropping its symbol table.
exitBlk :: AnalyserM ()
exitBlk = popSymTab

-- Start analysing a given method, pushing a new symbol table
-- with its formal parameters and the current method symbol.
-- Also resets reachability status, since the beginning of a function
-- is always reachable.
enterMthd :: Method Code -> AnalyserM ()
enterMthd m = do
    argsSyms <- mapM argToSym (mthdArgs m)
    pushSymTab
    -- The self symbol is added as part of enterCl, so we treat the method as if
    -- the induced first parameter did not exist.
    addSym $ Sym currentMthdSymIdent (mthdTypeIgnSelf m) (Just $ mthdCode m)
    addSyms argsSyms
    setReach (Reach True)
    where
      argToSym a@(Arg _ t i) = do
          unlessM (typeExists t) (nonexistentTypeError t a)
          when (isVoid t) (voidTypeExprError a)
          when (isVar t) (varArgError a)
          return $ Sym i (Ref () $ () <$ t) (Just $ toCode a)

-- Cleanup after analysing a method. Needs to analyse the return semantics
-- and then pop the methods symbol table.
exitMthd :: AnalyserM ()
exitMthd = do
  m <- getCurrentMthd
  let Fun _ r _ = symType m
  reach <- getReach
  when (isVar r) (varRetError (symCode m))
  unlessM (typeExists r) (nonexistentTypeErrorMb r (symCode m))
  -- The end of the method may be reachable only in a void method, which
  -- has an inferred empty return at its end.
  when (r /= Void () && isReachable reach) (noRetError (symCode m) r)
  popSymTab

analyseStmt :: Stmt Code -> AnalyserM (Stmt SemData)
analyseStmt stmt = do
  case stmt of
    Empty _ -> do
      semData <- analyseVoid stmt
      return $ Empty semData
    BStmt _ blk -> do
      blk' <- analyseBlk blk
      semData <- analyseVoid stmt
      return $ BStmt semData blk'
    Decl _ t items -> do
      items' <- mapM (analyseItemDecl (() <$ t)) items
      semData <- analyseVoid stmt
      return $ Decl semData (semData <$ t) items'
    Ass _ expr1 expr2 -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
      unless (isRef t1) (invAssError expr1' t1)
      unlessM (t2 `typeMatch` t1) (invTypeError expr2' t1 t2)
      semData <- analyseVoid stmt
      return $ Ass semData expr1' expr2'
    Incr _ i -> do
      mbSym <- getsSym (symTabLookup i)
      case mbSym of
        Nothing -> undeclError i stmt
        Just sym -> do
            let t = symType sym
            unlessM (t `typeMatch` Int ()) (invTypeError stmt (Int ()) t)
            semData <- analyseVoid stmt
            return $ Incr semData i
    Decr _ i -> do
      mbSym <- getsSym (symTabLookup i)
      case mbSym of
        Nothing -> undeclError i stmt
        Just sym -> do
            let t = symType sym
            unlessM (t `typeMatch` Int ()) (invTypeError stmt (Int ()) t)
            semData <- analyseVoid stmt
            return $ Decr semData i
    Ret _ expr -> do
      mthd <- getCurrentMthd
      expr' <- analyseExpr expr
      let Fun _ r1 _ = symType mthd
          r2 = semType $ unwrap expr'
      unlessM (r2 `typeMatch` r1) (invTypeError expr' r1 r2)
      semData <- analyseVoid stmt
      unreachable -- Code after a return statement is always unreachable.
      return $ Ret semData expr'
    VRet _ -> do
      mthd <- getCurrentMthd
      let Fun _ r1 _ = symType mthd
      unlessM (Void () `typeMatch` r1) (invTypeError stmt r1 (Void ()))
      semData <- analyseVoid stmt
      unreachable -- Code after a return statement is always unreachable.
      return $ VRet semData
    Cond _ expr stmt' -> do
      expr' <- analyseExpr expr
      let condType = semType $ unwrap expr'
      unlessM (condType `typeMatch` Bool ()) (invTypeError stmt' (Bool ()) condType)
      triviallyTrue <- isTriviallyTrue expr'
      stmt'' <- (if triviallyTrue then mustEnter else mayEnter) (analyseStmt stmt')
      semData <- analyseVoid stmt'
      return $ Cond semData expr' stmt''
    CondElse _ expr stmt1 stmt2 -> do
      expr' <- analyseExpr expr
      let condType = semType $ unwrap expr'
      unlessM (condType `typeMatch` Bool ()) (invTypeError stmt (Bool ()) condType)
      triviallyTrue <- isTriviallyTrue expr'
      triviallyFalse <- isTriviallyFalse expr'
      let analyser
            | triviallyTrue = mustEnterFirst
            | triviallyFalse = mustEnterSecond
            | otherwise = mustEnterOneOf2
      (stmt1', stmt2') <- analyser (analyseStmt stmt1) (analyseStmt stmt2)
      semData <- analyseVoid stmt
      return $ CondElse semData expr' stmt1' stmt2'
    While _ expr stmt' -> do
      expr' <- analyseExpr expr
      let condType = semType $ unwrap expr'
      unlessM (condType `typeMatch` Bool ()) (invTypeError stmt' (Bool ()) condType)
      triviallyTrue <- isTriviallyTrue expr'
      stmt'' <- (if triviallyTrue then mustEnter else mayEnter) (analyseStmt stmt')
      semData <- analyseVoid stmt'
      return $ While semData expr' stmt''
    For {} -> error "For should be rewritten before analysis."
    SExp _ expr -> do
      expr' <- analyseExpr expr
      semData <- analyseVoid stmt
      return $ SExp semData expr'

analyseItemDecl :: Type () -> Item Code -> AnalyserM (Item SemData)
analyseItemDecl t item = do
  unlessM (typeExists t) (nonexistentTypeError t item)
  when (isVoid t) (voidTypeExprError item)
  mbSym <- getsSym (symTabLocalScopeLookup i)
  case mbSym of
    Nothing -> case item of
      NoInit _ i' -> do
        when (isVar t) (varNoInitError item)
        addSym (Sym i' (Ref () t) (Just $ unwrap item))
        semData <- analyseVoid item
        return $ NoInit semData i'
      Init _ i' expr -> do
        expr' <- analyseExpr expr
        let exprType = semType $ unwrap expr'
            t' = if isVar t then exprType else t -- Infer type to the compile-time type of the expression.
        unlessM (exprType `typeMatch` t) (invTypeError expr' t exprType)
        addSym (Sym i' (Ref () t') (Just $ unwrap item))
        semData <- analyseVoid item
        return $ Init semData i' expr'
    Just sym -> conflDeclError item sym
  where
    i = case item of
      NoInit _ i' -> i'
      Init _ i' _ -> i'

analyseExpr :: Expr Code -> AnalyserM (Expr SemData)
analyseExpr srcExpr = case srcExpr of
    EVar _ i              -> do
        mbSym <- getsSym (symTabLookup i)
        case mbSym of
            Nothing -> undeclError i srcExpr
            Just sym -> do
                semData <- analyseTyped srcExpr (symType sym)
                return $ EVar semData i
    ELitInt _ n           -> do
        semData <- analyseTyped srcExpr (Int ())
        return $ ELitInt semData n
    EString _ s           -> do
        semData <- analyseTyped srcExpr (Str ())
        return $ EString semData s
    ELitTrue _            -> do
        semData <- analyseTyped srcExpr (Bool ())
        return $ ELitTrue semData
    ELitFalse _           -> do
        semData <- analyseTyped srcExpr (Bool ())
        return $ ELitFalse semData
    ENullI {} -> error "ENullI should be rewritten to ENull before analysis."
    ENullArr {} -> error "ENullArr should be rewritten to ENull before analysis."
    ENull _ t -> do
        unlessM (typeExists t) (nonexistentTypeError t srcExpr)
        when (isVoid t) (voidTypeExprError srcExpr)
        tSemData <- analyseVoid t
        exprSemData <- analyseTyped srcExpr (() <$ t)
        return $ ENull exprSemData (tSemData <$ t)
    ENew _ t              -> do
        when (isVar t) (varNewError srcExpr)
        unlessM (typeExists t) (nonexistentTypeError t srcExpr)
        when (isVoid t) (voidTypeExprError srcExpr)
        tSemData <- analyseVoid t
        exprSemData <- analyseTyped srcExpr (() <$ t)
        return $ ENew exprSemData (tSemData <$ t)
    ENewArr _ t expr      -> do
        when (isVar t) (varNewError srcExpr)
        unlessM (typeExists t) (nonexistentTypeError t srcExpr)
        when (isVoid t) (voidTypeExprError srcExpr)
        tSemData <- analyseVoid t
        expr' <- analyseExpr expr
        exprSemData <- analyseTyped srcExpr (Arr () (() <$ t))
        return $ ENewArr exprSemData (tSemData <$ t) expr'
    EApp _ expr args      -> do
        expr' <- analyseExpr expr
        args' <- mapM analyseExpr args
        let t = semType $ unwrap expr'
        case t of
            Fun _ r ps -> do
                let nargs = length args'
                    nps = length ps
                    argsPs = zip args' ps
                unless (nps == nargs) (mismatchedNumArgsError srcExpr nps nargs)
                mismatched <- filterM (\(a, p) -> not <$> semType (unwrap a) `typeMatch` p) argsPs
                unless (null mismatched) (mismatchedArgsError srcExpr mismatched)
                exprSemData <- analyseTyped srcExpr r
                return $ EApp exprSemData expr' args'
            _          -> nonfnAppError srcExpr t
    EIdx _ expr idxExpr   -> do
      expr' <- analyseExpr expr
      idxExpr' <- analyseExpr idxExpr
      let et = semType $ unwrap expr'
          idxt = semType $ unwrap idxExpr'
      unlessM (idxt `typeMatch` Int ()) (invTypeError idxExpr (Int ()) idxt)
      case deref et of
        Arr _ t -> do
          semData <- analyseTyped srcExpr (Ref () t)
          return $ EIdx semData expr' idxExpr'
        _ -> notArrIdxError expr et
    EAcc _ expr i         -> do
      expr' <- analyseExpr expr
      sym <- accessMember (semType $ unwrap expr') i srcExpr
      semData <- analyseTyped srcExpr (symType sym)
      return $ EAcc semData expr' i
    ENeg _ expr           -> do
      expr' <- analyseExpr expr
      let t = semType $ unwrap expr'
      unlessM (t `typeMatch` Int ()) (invTypeError expr (Int ()) t)
      semData <- analyseTyped srcExpr (Int ())
      return $ ENeg semData expr'
    ENot _ expr           -> do
      expr' <- analyseExpr expr
      let t = semType $ unwrap expr'
      unlessM (t `typeMatch` Bool ()) (invTypeError expr (Bool ()) t)
      semData <- analyseTyped srcExpr (Bool ())
      return $ ENot semData expr'
    EMul _ expr1 op expr2 -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
      unlessM (t1 `typeMatch` Int ()) (invTypeError expr1 (Int ()) t1)
      unlessM (t2 `typeMatch` Int ()) (invTypeError expr2 (Int ()) t2)
      semData <- analyseTyped srcExpr (Int ())
      opSemData <- analyseVoid op
      return $ EMul semData expr1' (opSemData <$ op) expr2'
    EAdd _ expr1 op expr2 -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
      case op of
        Plus _   -> unless (areAddTypes t1 t2) (nonaddTypeError srcExpr t1 t2)
        Minus  _ -> unless (areMinTypes t1 t2) (nonminTypeError srcExpr t1 t2)
      semData <- analyseTyped srcExpr t1
      opSemData <- analyseVoid op
      return $ EAdd semData expr1' (opSemData <$ op) expr2'
    ERel _ expr1 op expr2 -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
          eqOp = unlessM (areEqTypes t1 t2) (noneqTypeError srcExpr t1 t2)
          cmpOp = unless (areCmpTypes t1 t2) (noncmpTypeError srcExpr t1 t2)
      case op of
        EQU _ -> eqOp
        NE  _ -> eqOp
        _     -> cmpOp
      semData <- analyseTyped srcExpr (Bool ())
      opSemData <- analyseVoid op
      return $ ERel semData expr1' (opSemData <$ op) expr2'
    EAnd _ expr1 expr2    -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
      unlessM (t1 `typeMatch` Bool ()) (invTypeError expr1 (Bool ()) t1)
      unlessM (t2 `typeMatch` Bool ()) (invTypeError expr2 (Bool ()) t2)
      semData <- analyseTyped srcExpr (Bool ())
      return $ EAnd semData expr1' expr2'
    EOr _ expr1 expr2     -> do
      expr1' <- analyseExpr expr1
      expr2' <- analyseExpr expr2
      let t1 = semType $ unwrap expr1'
          t2 = semType $ unwrap expr2'
      unlessM (t1 `typeMatch` Bool ()) (invTypeError expr1 (Bool ()) t1)
      unlessM (t2 `typeMatch` Bool ()) (invTypeError expr2 (Bool ()) t2)
      semData <- analyseTyped srcExpr (Bool ())
      return $ EOr semData expr1' expr2'

isTriviallyTrue :: Expr SemData -> AnalyserM Bool
isTriviallyTrue expr = case expr of
  ELitTrue _ -> return True
  _          -> return False

isTriviallyFalse :: Expr SemData -> AnalyserM Bool
isTriviallyFalse expr = case expr of
  ELitFalse _ -> return True
  _           -> return False

topLevelSymTab :: Metadata a -> SymbolTable
topLevelSymTab (Meta cls) =
  let topLevelCl = cls Map.! topLevelClassIdent
      topLevelSyms = map mthdToSym (clMethods topLevelCl) ++ nativeTopLevelSymbols
   in SymTab (Map.fromList $ map (\s -> (symName s, s)) topLevelSyms) Nothing

-- Annotate the piece of syntax with semantic data according to current state and a Void type.
analyseVoid :: Unwrappable f => f Code -> AnalyserM SemData
analyseVoid x = analyseTyped x (Void ())

-- Annotate the piece of syntax with semantic data according to current state and the given type.
analyseTyped :: Unwrappable f => f Code -> Type () -> AnalyserM SemData
analyseTyped x t = do
  syms <- getSyms
  reach <- getReach
  let semData = SemData syms t (Just $ unwrap x) reach
  return semData

getsCls :: (ClassStore -> a) -> AnalyserM a
getsCls f = gets (f . stClasses)

getsSym :: (SymbolTable -> a) -> AnalyserM a
getsSym f = gets (f . stSymTab)

getSyms :: AnalyserM SymbolTable
getSyms = getsSym id

-- Set reachability state to unreachable.
unreachable :: AnalyserM ()
unreachable = setReach (Reach False)

asksCl :: Ident -> AnalyserM (Maybe (Class Code))
asksCl i = asks (\(Meta m) -> Map.lookup i m)

modifyCl :: (ClassStore -> ClassStore) -> AnalyserM ()
modifyCl f = modify (\s -> s {stClasses = f $ stClasses s})

modifySym :: (SymbolTable -> SymbolTable) -> AnalyserM ()
modifySym f = modify (\s -> s {stSymTab = f $ stSymTab s})

-- Store an analysed class in the state.
storeClass :: Class SemData -> AnalyserM ()
storeClass cl = modifyCl (Map.insert (clName cl) cl)

-- Push a new, empty symbol table on the symbol table stack as a child of the current symbol table.
pushSymTab :: AnalyserM ()
pushSymTab = modifySym $ SymTab Map.empty . Just

-- Remove the current symbol table and set its parent as the current one.
popSymTab :: AnalyserM ()
popSymTab = modifySym $ fromJust . symParent

-- Add given symbols to the current symbol table.
addSyms :: [Symbol] -> AnalyserM ()
addSyms syms = modifySym (symTabUnion $ Map.fromList $ map (\s -> (symName s, s)) syms)

-- Add the given symbol to the current symbol table.
addSym :: Symbol -> AnalyserM ()
addSym sym = modifySym (symTabInsert sym)

symTabUnion :: Map.Map Ident Symbol -> SymbolTable -> SymbolTable
symTabUnion x s =
  let y = symTab s
   in s {symTab = Map.union x y}

-- Add the given symbol to the given symbol table.
symTabInsert :: Symbol -> SymbolTable -> SymbolTable
symTabInsert sym s = s {symTab = Map.insert (symName sym) sym (symTab s)}

-- Get a given symbol from the symbol table stack, recursivelly.
symTabGet :: Ident -> SymbolTable -> Symbol
symTabGet i s = fromJust $ symTabLookup i s

-- Lookup a given symbol in the symbol table stack, recursivelly.
symTabLookup :: Ident -> SymbolTable -> Maybe Symbol
symTabLookup i s =
  let mbSym = Map.lookup i (symTab s)
   in case (mbSym, symParent s) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just s') -> symTabLookup i s'
        (Just sym, _)      -> Just sym

-- Lookup a given symbol in the current symbol table; does not look recursivelly through the stack.
symTabLocalScopeLookup :: Ident -> SymbolTable -> Maybe Symbol
symTabLocalScopeLookup i s = Map.lookup i (symTab s)

-- Get the symbol representing the current method.
getCurrentMthd :: AnalyserM Symbol
getCurrentMthd = getsSym (symTabGet currentMthdSymIdent)

-- Convert field metadata into a symbol.
fldToSym :: Field -> Symbol
fldToSym fld = Sym (fldName fld) (Ref () $ fldType fld) (Just $ toCode $ fldCode fld)

-- Convert method metadata into a symbol.
mthdToSym :: Method a -> Symbol
mthdToSym m = Sym (mthdName m) (mthdTypeIgnSelf m) (Just $ mthdCode m)

-- Check if a given type is declared in the metadata.
typeExists :: Type a -> AnalyserM Bool
typeExists t = case deref t of
  Cl _ i  -> do
    mbcl <- asksCl i
    return $ isJust mbcl
  _       -> return True

isVoid :: Type a -> Bool
isVoid t = case deref t of
  Void _ -> True
  _      -> False

isVar :: Type a -> Bool
isVar t = case deref t of
  Var _    -> True
  Arr _ t' -> isVar t'
  _        -> False

-- Can an expression of type t1 be used in a place where the type t2 is required.
-- Both types are dereferenced, so Ref has no impact on the result.
typeMatch :: Type a -> Type b -> AnalyserM Bool
typeMatch t1 t2 = case (deref t1, deref t2) of
  (Int _, Int _)         -> return True
  (Str _, Str _)         -> return True
  (Bool _, Bool _)       -> return True
  (Void _, Void _)       -> return True
  (_, Var _)             -> return True
  (Arr _ t1', Arr _ t2') -> typeMatch t1' t2'
  (Cl _ i1, Cl _ i2)     -> do
    cl1 <- asksCl i1
    cl2 <- asksCl i2
    return $ fromJust cl1 `classMatch` fromJust cl2
  (Fun _ r1 ps1, Fun _ r2 ps2) -> do
    rMatch <- typeMatch r1 r2
    if not rMatch || length ps1 /= length ps2
      then return False
      else do
        pMatches <- zipWithM typeMatch ps1 ps2
        return $ and pMatches
  _                      -> return False

-- Does the language support addition of expressions of given types.
areAddTypes :: Type a -> Type b -> Bool
areAddTypes t1 t2 = case (deref t1, deref t2) of
  (Int _, Int _) -> True
  (Str _, Str _) -> True
  _              -> False

-- Does the language support subtraction of expressions of given types.
areMinTypes :: Type a -> Type b -> Bool
areMinTypes t1 t2 = case (deref t1, deref t2) of
  (Int _, Int _) -> True
  _              -> False

-- Does the language support equation of expressions of given types.
areEqTypes :: Type a -> Type b -> AnalyserM Bool
areEqTypes t1 t2 = liftM2 (||) (t1 `typeMatch` t2 ) (t2 `typeMatch` t1)

-- Does the language support relational comparison of expressions of given types.
areCmpTypes :: Type a -> Type b -> Bool
areCmpTypes t1 t2 = case (deref t1, deref t2) of
  (Int _, Int _)   -> True
  (Str _, Str _)   -> True
  (Bool _, Bool _) -> True
  _                -> False

-- Can an instance of cl1 be used where an instance of cl2 is required.
-- In other words, is cl1 a subclass of cl2 or cl2.
classMatch :: Class a -> Class a -> Bool
classMatch cl1 cl2 =
  (clName cl1 == clName cl2) || case clBase cl1 of
    Nothing   -> False
    Just cl1' -> classMatch cl1' cl2

-- Resolve the access to a member with a given identifier on an expression of given type.
-- If access is impossible due to whatever reason, the monad will fail with a suitable error.
accessMember :: (Positioned a, Error.WithContext a, Unwrappable f) => Type () -> Ident -> f a -> AnalyserM Symbol
accessMember t i ctx = case t of
  Arr _ _ -> if i == arrayLengthIdent then return $ Sym arrayLengthIdent (Int ()) undefined
                                    else invAccessError t i ctx
  Cl _ cli  -> do
    mcl <- asksCl cli
    case mcl of
      Nothing -> undeclTypeError t ctx
      Just cl -> let fld = find (\f -> fldName f == i) (clFields cl)
                     mthd = find (\m -> mthdName m == i) (clMethods cl)
                 in case (fld, mthd) of
                      (Just fld', _)     -> return $ fldToSym fld'
                      (_, Just mthd')    -> return $ mthdToSym mthd'
                      (Nothing, Nothing) -> invAccessError t i ctx
  Ref _ t' -> accessMember t' i ctx
  _       -> invAccessError t i ctx

isRef :: Type a -> Bool
isRef t = case t of
  Ref _ _ -> True
  _       -> False

-- Remove all layers of Ref from a given type.
-- The language typing should never cause a type like:
-- Ref (Ref ... (NotRef (Ref (Ref ...))))
-- i.e. with two different layers of indirection separated by some other type.
-- So, for example, a type `int&[]&` should be impossible.
deref :: Type a -> Type a
deref t = case t of
  Ref _ t' -> deref t'
  _        -> t

-- Symbols that are linked from the native library.
nativeTopLevelSymbols :: [Symbol]
nativeTopLevelSymbols = [printInt, printString, error_, readInt, readString]
    where printInt = Sym (Ident "printInt") (Fun () (Void ()) [Int ()]) Nothing
          printString = Sym (Ident "printString") (Fun () (Void ()) [Str ()]) Nothing
          error_ = Sym (Ident "error") (Fun () (Void ()) []) Nothing
          readInt = Sym (Ident "readInt") (Fun () (Int ()) []) Nothing
          readString = Sym (Ident "readString") (Fun () (Str ()) []) Nothing

-- Errors

raise :: String -> AnalyserM a
raise = lift . lift . Left

conflDeclError :: Item Code -> Symbol -> AnalyserM a
conflDeclError item sym = raise $ Error.errorMsg msg (unwrap item) ctx
  where
    msg = "Conflicting declarations in the same scope."
    ctx =
      "In declaration of `" ++ showI (symName sym) ++ "`.\n"
        ++ Error.lineInfo (symCode sym >>= pos)
        ++ ": previously declared here."

invTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> Type () -> AnalyserM b
invTypeError ctx expected actual = raise $ Error.errorCtxMsg msg ctx
  where
    msg = "Invalid expression type. Expected `" ++ showType expected ++ "`, found `" ++ showType actual ++ "`."

undeclError :: (Positioned a, Error.WithContext a, Unwrappable f) => Ident -> f a -> AnalyserM b
undeclError i ctx = raise $ Error.errorCtxMsg msg ctx
  where
    msg = "Undeclared identifier `" ++ showI i ++ "`."

nonfnAppError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> AnalyserM b
nonfnAppError ctx t = raise $ Error.errorCtxMsg msg ctx
    where msg = "Invalid call to a non-function type `" ++ showType t ++ "`."

noneqTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> Type () -> AnalyserM b
noneqTypeError ctx t1 t2 = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot equate expressions of types `" ++ showType t1 ++ "` and `" ++ showType t2 ++ "`. "
              ++ "Only expressions of types within the same inheritance chain can be equated."

noncmpTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> Type () -> AnalyserM b
noncmpTypeError ctx t1 t2 = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot compare expressions of types `" ++ showType t1 ++ "` and `" ++ showType t2 ++ "`. "
              ++ "Only two expressions of the same type out of `int`, `string` and `bool` can be compared."

nonaddTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> Type () -> AnalyserM b
nonaddTypeError ctx t1 t2 = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot add expressions of types `" ++ showType t1 ++ "` and `" ++ showType t2 ++ "`. "
              ++ "Only two expressions of type `int` or two expressions of type `string` can be added."

nonminTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> Type () -> AnalyserM b
nonminTypeError ctx t1 t2 = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot subtract expressions of types `" ++ showType t1 ++ "` and `" ++ showType t2 ++ "`. "
              ++ "Only `int` expressions can be subtracted."

mismatchedArgsError :: (Positioned a, Error.WithContext a, Unwrappable f,
                        Unwrappable g, Print (g SemData)) => f a -> [(g SemData, Type ())] -> AnalyserM b
mismatchedArgsError ctx mismatched = raise $ Error.errorCtxMsg msg ctx
    where
        msg = "Mismatched types of arguments in function call.\n" ++ intercalate "\n" ctxs
        ctxs = map (uncurry mismatchCtx) mismatched
        mismatchCtx e t = "Argument `" ++ printTree e ++ "` has invalid type `" ++ showType (semType $ unwrap e)
                        ++ "`. Expected type `" ++ showType t ++ "`."

mismatchedNumArgsError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Int -> Int -> AnalyserM b
mismatchedNumArgsError ctx nparams nargs = raise $ Error.errorCtxMsg msg ctx
    where msg = "Mismatched number of arguments in function call. Expected " ++ show nparams ++ ", found " ++ show nargs ++ "."

notArrIdxError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> AnalyserM b
notArrIdxError ctx t = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot index to a non-array type `" ++ showType t ++ "`."

noRetError :: Maybe Code -> Type () -> AnalyserM a
noRetError c t = raise $ Error.errorMsgMb msg (c >>= codePos) (codeString <$> c)
    where msg = "Control may reach the end of a non-void function without a return statement. Expected return type: `" ++ showType t ++ "`."

invAccessError :: (Positioned a, Error.WithContext a, Unwrappable f) => Type () -> Ident -> f a -> AnalyserM b
invAccessError t i ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot access member `" ++ showI i ++ "` of type `" ++ showType t ++ "`."

undeclTypeError :: (Positioned a, Error.WithContext a, Unwrappable f) => Type () -> f a -> AnalyserM b
undeclTypeError t ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Undeclared type `" ++ showType t ++ "`."

invAssError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> Type () -> AnalyserM b
invAssError ctx t = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot assign to a value of type `" ++ showType t ++ "`."

nonexistentTypeError :: (Positioned b, Error.WithContext b, Unwrappable f) => Type a -> f b -> AnalyserM c
nonexistentTypeError t ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Use of undeclared type `" ++ showType t ++ "`."

nonexistentTypeErrorMb :: Type a -> Maybe Code -> AnalyserM c
nonexistentTypeErrorMb t c = raise $ Error.errorMsgMb msg (c >>= codePos) (codeString <$> c)
    where msg = "Use of undeclared type `" ++ showType t ++ "`."

voidTypeExprError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> AnalyserM b
voidTypeExprError ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot use `void` as a type of a value."

voidFldError :: Maybe Code -> AnalyserM a
voidFldError c = raise $ Error.errorMsgMb msg (c >>= codePos) (codeString <$> c)
    where msg = "Cannot use `void` as a type of a field."

varNoInitError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> AnalyserM b
varNoInitError ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot declare a `var` type variable without initialization."

varRetError :: Maybe Code -> AnalyserM a
varRetError c = raise $ Error.errorMsgMb msg (c >>= codePos) (codeString <$> c)
    where msg = "Cannot use `var` as the return type of a function."

varArgError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> AnalyserM b
varArgError ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot use `var` as the type of a function parameter."

varNewError :: (Positioned a, Error.WithContext a, Unwrappable f) => f a -> AnalyserM b
varNewError ctx = raise $ Error.errorCtxMsg msg ctx
    where msg = "Cannot use `var` in a `new` expression."

varFldError :: Maybe Code -> AnalyserM a
varFldError c = raise $ Error.errorMsgMb msg (c >>= codePos) (codeString <$> c)
    where msg = "Cannot use `var` as the type of a field."
