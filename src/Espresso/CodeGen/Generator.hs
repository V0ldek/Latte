module Espresso.CodeGen.Generator (generateEspresso) where

import           Control.Monad.Reader
import           Data.List                  (foldl')
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
import           Espresso.CodeGen.GenM
import           Espresso.CodeGen.Labels
import           Espresso.CodeGen.Operators
import           Espresso.Syntax.Abs        as Espresso
import           Espresso.Types
import           Espresso.Utilities
import           Identifiers
import           SemanticAnalysis.Analyser  (SemData (..), Symbol (..),
                                             SymbolTable (..), symTabLookup)
import           SemanticAnalysis.Class     as Class
import           SemanticAnalysis.TopLevel  as TopLevel (Metadata (..))
import qualified Syntax.Abs                 as Latte
import           Syntax.Code                (Code)
import           Utilities                  (dedupBy, single)

-- Generate an Espresso program from a semantically analysed Latte program.
generateEspresso :: TopLevel.Metadata SemData -> Program ()
generateEspresso (TopLevel.Meta meta) =
    let cls = Map.elems meta
        preamble = Espresso.Meta () $ map genClDef cls
        code = map genMthd $ dedupBy mthdQIdent $ concatMap clMethods cls
    in  Program () preamble code

-- Generate an Espresso .class definition for a semantically analysed Latte class.
genClDef :: Class.Class SemData -> Espresso.ClassDef ()
genClDef cl = ClDef () (toSymIdent $ clName cl)
                (map emitFldDef $ clFields cl)
                (map emitMthdDef $ clMethods cl)
    where
        emitFldDef fld = FldDef () (toSType $ fldType fld) (toSymIdent $ fldName fld)
        emitMthdDef mthd = MthdDef () (toFType $ mthdType mthd) (mthdQIdent mthd)

-- Generate an Espresso method definition for .methods in a .class metadata segment
-- from a semantically analysed Latte class.
genMthd :: Class.Method SemData -> Espresso.Method ()
genMthd mthd =
    let code = runGen go
        params = map (\(Latte.Arg _ t (Latte.Ident i)) ->
            Param () (toSType $ () <$ t) (argValIdent i)) (mthdArgs mthd)
    in Espresso.Mthd () (toSType $ () <$ mthdRet mthd) (mthdQIdent mthd) params code
    where
        go = do
            let Latte.Block _ stmts = mthdBlk mthd
                decls = declArgs $ mthdArgs mthd
            emit $ mbAnnLabel entryLabel (codeLines (mthdBlk mthd))
            localSyms decls (genStmts stmts)
            code <- getCode
            if mthdRet mthd == Latte.Void ()
                then return $ unifyVRet code
                else unifyRet (mthdRet mthd) code

-- Generate Espresso code for a block of Latte statements.
genStmts :: [Latte.Stmt SemData] -> GenM ()
genStmts [] = return ()
genStmts (stmt : stmts) = case stmt of
    Latte.Empty _     -> return ()
    Latte.BStmt _ (Latte.Block _ stmts') -> do
        -- Scoping: save the environment, generate the block,
        -- restore the environment to before-the-block.
        env <- ask
        genStmts stmts'
        local (const env) (genStmts stmts)
    Latte.Decl _ _ items -> do
        let syms = semSymbols $ single stmt
        decls <- genItems syms items
        localSyms decls (genStmts stmts)
    Latte.Ass _ e1 e2 -> do
        (lvalue, type_) <- genLValue e1
        rvalue <- genExpr e2
        case type_ of
            VLocal    -> emit $ ISet () (valName lvalue) rvalue
            -- Future-proof for fields and arrays.
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
    {-
        ```lat
        if (cond) { <body> }
        <rest_of_code>
        ```
        ```esp
        <cond code>
        .L_then:
          <body>
          jump .L_after;  // Redundant jump for easier CFG generation.
        .L_after:
          <rest_of_code>
    -}
    Latte.Cond _ cond stmtTrue -> do
        (lthen, lafter) <- condLabels
        genCond cond lthen lafter
        emit $ mbAnnLabel lthen (codeLines stmtTrue)
        genStmts [stmtTrue]
        emit $ IJmp () lafter
        emit $ ILabel () lafter
        genStmts stmts
    {-
        ```lat
        if (cond) { <then_body> }
        else      { <else_body> }
        <rest_of_code>
        ```
        ```esp
        <cond code>
        .L_then:
          <then_body>
          jump .L_after;
        .L_else:
          <rest_of_code>
          jump .L_after;  // Redundant jump for easier CFG generation.
        .L_after:
          <rest_of_code>
    -}
    Latte.CondElse _ cond stmtTrue stmtFalse -> do
        (lthen, lelse, lafter) <- condElseLabels
        genCond cond lthen lelse
        emit $ mbAnnLabel lthen (codeLines stmtTrue)
        genStmts [stmtTrue]
        emit $ IJmp () lafter
        emit $ mbAnnLabel lelse (codeLines stmtFalse)
        genStmts [stmtFalse]
        emit $ IJmp () lafter
        emit $ ILabel () lafter
        genStmts stmts
    {-
        ```lat
        while (cond) { <loop_body> }
        <rest_of_code>
        ```
        ```esp
        jump .L_cond;
        .L_body
          <loop_body>;
          jump .L_cond;  // Redundant jump for easier CFG generation.
        .L_cond
          <cond code> // Jump .L_body if true, .L_after if not.
        .L_after:
          <rest_of_code>
    -}
    Latte.While _ cond body -> do
        (lcond, lbody, lafter) <- whileLabels
        emit $ IJmp () lcond
        emit $ mbAnnLabel lbody (codeLines body)
        genStmts [body]
        emit $ IJmp () lcond
        emit $ mbAnnLabel lcond (codeLines cond)
        genCond cond lbody lafter
        emit $ ILabel () lafter
        genStmts stmts
    Latte.For {} -> error "for should be rewritten before Espresso codegen"
    Latte.SExp _ e          -> do
        _ <- genExpr e
        genStmts stmts

-- Generate code for item declarations, evaluating initialisers based on a symbol table.
-- For declarations without initialisers emits a declaration to the type's default value.
genItems :: SymbolTable -> [Latte.Item SemData] -> GenM [(Latte.Ident, EspVal)]
genItems syms = mapM genItem
    where genItem item = do
            (i, val) <- genVal item
            let t = single val
            vi <- valIdentFor i
            emit $ ISet () vi (() <$ val)
            return (i, EspVal vi t)
          genVal :: Latte.Item SemData -> GenM (Latte.Ident, Val (SType ()))
          genVal item = case item of
            Latte.NoInit _ i -> do
                let sym = fromJust $ symTabLookup i syms
                    t = toSType $ symType sym
                    val = defaultVal t
                return (i, t <$ val)
            Latte.Init _ i e -> do
                let sem = single e
                    t = toSType $ semType sem
                val <- genExpr e
                return (i, t <$ val)

-- Turn method's arguments into Espresso values
declArgs :: [Latte.Arg Code] -> [(Latte.Ident, EspVal)]
declArgs = map declArg
    where
    declArg (Latte.Arg _ t i) = (i, EspVal (argValIdent $ toStr i) (toSType $ () <$ t))

-- Generate Espresso code for a Latte expression.
genExpr :: Latte.Expr SemData -> GenM (Val ())
genExpr expr = case expr of
    Latte.EVar _ i        -> do
        val <- askSym i
        return (toVVal val)
    Latte.ELitInt _ n     -> return $ VInt () n
    -- String literals are special and emit a separate instruction
    -- so that assembly codegen can emit an allocation from a string constant.
    Latte.EString _ s     -> do
        newval <- freshVal
        emit $ IStr () newval s
        return $ VVal () (Ref () (Str ())) newval
    Latte.ELitTrue _      -> return $ VTrue ()
    Latte.ELitFalse _     -> return $ VFalse ()
    Latte.ENullI {}       -> error "ENullI should be converted to ENull before Espresso codegen"
    Latte.ENullArr {}     -> error "ENullArr should be converted to ENull before Espresso codegen"
    Latte.ENull _ _       -> return $ VNull ()
    Latte.ENew {}         -> error "objects unimplemented"
    Latte.ENewArr {}      -> error "arrays unimplemented"
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
    {-
    ```lat
    a && b
    ```
    ```esp
      %v_r := false;
      <cond code>    // Jump to .L_true if true, .L_false if false.
    .L_true:
      %v_r := true;
      jump .L_false; // Redundant jump for easier CFG generation.
    .L_false:
      <nothing, %v_r set to false>
    ```
    -}
    Latte.EAnd {}    -> do
        (ltrue, lfalse) <- andLabels
        newval <- freshVal
        emit $ ISet () newval (VFalse ())
        genCond expr ltrue lfalse
        emit $ ILabel () ltrue
        emit $ ISet () newval (VTrue ())
        emit $ IJmp () lfalse
        emit $ ILabel () lfalse
        return $ VVal () (Bool ()) newval
    {-
    ```lat
    a || b
    ```
    ```esp
      %v_r := true;
      <cond code>    // Jump to .L_true if true, .L_false if false.
    .L_false:
      %v_r := false;
      jump .L_true; // Redundant jump for easier CFG generation.
    .L_true:
      <nothing, %v_r set to false>
    ```
    -}
    Latte.EOr {}     -> do
        (ltrue, lfalse) <- orLabels
        newval <- freshVal
        emit $ ISet () newval (VTrue ())
        genCond expr ltrue lfalse
        emit $ ILabel () lfalse
        emit $ ISet () newval (VFalse ())
        emit $ IJmp () ltrue -- Redundant jump for easier CFG generation.
        emit $ ILabel () ltrue
        return $ VVal () (Bool ()) newval

-- Generate code for a Latte expression interpreted as a boolean condition.
-- Equivalent to a conditional jump to the first label if the condition is true
-- or to the second if it is false.
genCond :: Latte.Expr SemData -> LabIdent -> LabIdent -> GenM ()
genCond e ltrue lfalse = case e of
    {-
    Short-circuting AND.
    ```lat
    a && b
    ```
    ```esp
      <code for condition a> // Jump to .L_mid if true, .L_false if false.
    .L_mid:
      <code for condition b> // Jump to .L_true if true, .L_false if false.
    ```
    -}
    Latte.EAnd _ e1 e2 -> do
        lmid <- freshLabel
        genCond e1 lmid lfalse
        emit $ ILabel () lmid
        genCond e2 ltrue lfalse
    {-
    Short-circuting OR.
    ```lat
    a || b
    ```
    ```esp
      <code for condition a> // Jump to .L_true if true, .L_mid if false.
    .L_mid:
      <code for condition b> // Jump to .L_true if true, .L_false if false.
    ```
    -}
    Latte.EOr _ e1 e2 -> do
        lmid <- freshLabel
        genCond e1 ltrue lmid
        emit $ ILabel () lmid
        genCond e2 ltrue lfalse
    _ -> do
        val <- genExpr e
        emit $ ICondJmp () val ltrue lfalse

-- Generate code for the given unary operator applied to an expression.
genUnOp :: Latte.Expr SemData -> UnOp () -> GenM (Val ())
genUnOp e op = do
    let t = semType $ single e
    val <- genExpr e
    newval <- freshVal
    emit $ IUnOp () newval op val
    return $ VVal () (toSType t) newval

-- Generate code for the given binary operator applied to given expressions.
genOp :: Latte.Expr SemData -> Latte.Expr SemData -> Op () -> GenM (Val ())
genOp e1 e2 op = do
    let t = semType $ single e1
    val1 <- genExpr e1
    val2 <- genExpr e2
    newval <- freshVal
    emit $ IOp () newval val1 op val2
    return $ VVal () (toSType t) newval

-- Create a special .L_exit label signifying the end of the method
-- and turn all void returns to jumps to that label.
-- Additionally add a jump at end of the initial instruction sequence
-- in case the return was implicit. This may result in two consecutive jump
-- instructions if there was an explicit return, but this will be removed
-- as dead code in later phases.
unifyVRet :: [Instr ()] -> [Instr ()]
unifyVRet instrs =
    let instr' = map retToJmp instrs
    in instr' ++ [IJmp () exitLabel, ILabel () exitLabel, IVRet ()]
    where
        retToJmp (IVRet ()) = IJmp () exitLabel
        retToJmp instr      = instr

-- Create a special .L_exit label signifying the end of the method
-- and turn all returns to jumps to that label. The return value
-- is propagated to a phi instruction at the start of .L_exit.
-- Additionally add a jump at the end of the initial instruction sequence.
-- There are no implicit returns in this case, but there might be an empty,
-- unreachable label. For example, when we had an if-else instruction in
-- which both branches return, there will be an empty .L_after label created
-- for that condition. Since the label is unreachable it will be removed as
-- dead code, but to maintain the property that each basic block ends with a jump
-- we need this artificial jump.
unifyRet :: Latte.Type () -> [Instr ()] -> GenM [Instr ()]
unifyRet t instrs = do
    let (phi, _) = foldl' (flip goPhis) ([], entryLabel) instrs
        instrs' = map retToJmp instrs
    val <- valIdentFor (Latte.Ident "ret")
    return $ instrs' ++ [
            IJmp () exitLabel,
            ILabel () exitLabel,
            IPhi () val phi,
            IRet () (VVal () (toSType t) val)
        ]
    where
        goPhis (ILabel _ l) (phi, _)        = (phi, l)
        goPhis (ILabelAnn _ l _ _) (phi, _) = (phi, l)
        goPhis (IRet _ v) (phi, l)          = (PhiVar () l v:phi, l)
        goPhis _ x                          = x
        retToJmp IRet {} = IJmp () exitLabel
        retToJmp i       = i

data ValType = VLocal | VIndirect

genLValue :: Latte.Expr SemData -> GenM (EspVal, ValType)
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

toVVal :: EspVal -> Val ()
toVVal (EspVal i t) = VVal () t i
