-- The syntactic rewrite phase.
-- Desugaring and simplification of source code.
module Syntax.Rewriter (rewrite) where

import           Identifiers
import           Syntax.Abs
import           Syntax.Code
import           Syntax.Printer

-- Desugar and simplify the input program while retaining
-- the exact source code and position information inside the tree.
rewrite :: Program Pos -> Program Code
rewrite p@(Program _ tds) = let tds' = map rewriteTopDef tds
                            in Program (toCode p) tds'

rewriteTopDef :: TopDef Pos -> TopDef Code
rewriteTopDef td = case td of
    FnDef _ t i args blk -> let t' = rewriteType t
                                args' = map rewriteArg args
                                blk' = rewriteBlock blk
                            in FnDef (toCode td) t' i args' blk'
    ClDef _ i clblk -> let clblk' = rewriteClBlock clblk
                       in ClDef (toCode td) i clblk'
    ClExtDef _ i ext clblk -> let clblk' = rewriteClBlock clblk
                              in  ClExtDef (toCode td) i ext clblk'

rewriteBlock :: Block Pos -> Block Code
rewriteBlock x@(Block _ stmts) = let stmts' = map rewriteStmt stmts
                                 in  Block (toCode x) stmts'

rewriteClBlock :: ClBlock Pos -> ClBlock Code
rewriteClBlock x@(ClBlock _ cldefs) = let cldefs' = map rewriteClDef cldefs
                                      in  ClBlock (toCode x) cldefs'

rewriteClDef :: ClDef Pos -> ClDef Code
rewriteClDef cldef = case cldef of
    MthDef _ t i args blk -> let t' = rewriteType t
                                 args' = map rewriteArg args
                                 blk' = rewriteBlock blk
                            in MthDef (toCode cldef) t' i args' blk'
    FldDef _ t i -> let t' = rewriteType t
                    in  FldDef (toCode cldef) t' i

rewriteStmt :: Stmt Pos -> Stmt Code
rewriteStmt stmt =
    let code = toCode stmt
    in case stmt of
    Empty _           -> Empty code
    BStmt _ blk       -> BStmt code (rewriteBlock blk)
    Decl _ t items    -> Decl code (rewriteType t) (map rewriteItem items)
    Ass _ expr1 expr2 -> Ass code (rewriteExpr expr1) (rewriteExpr expr2)
    Incr _ i          -> Incr code i
    Decr _ i          -> Decr code i
    Ret _ expr        -> Ret code (rewriteExpr expr)
    VRet _            -> VRet code
    Cond _ expr stmt  -> Cond code (rewriteExpr expr) (blkWrap $ rewriteStmt stmt)
    CondElse _ expr stmt1 stmt2 -> CondElse code (rewriteExpr expr) (blkWrap $ rewriteStmt stmt1) (blkWrap $ rewriteStmt stmt2)
    While _ expr stmt -> While code (rewriteExpr expr) (blkWrap $ rewriteStmt stmt)
    For _ t i expr stmt -> let expr' = rewriteExpr expr
                               stmt' = rewriteStmt stmt
                               t'    = rewriteType t
                               exprCode = unwrap expr'
                               stmtCode = unwrap stmt'
                               tCode    = unwrap t'
                               arrDecl = Decl exprCode (Var exprCode) [Init exprCode forArrayIdent expr']
                               idxDecl = Decl exprCode (Int exprCode) [Init exprCode forIndexIdent (ELitInt exprCode 0)]
                               arrVar = EVar exprCode forArrayIdent
                               idxVar = EVar exprCode forIndexIdent
                               lenExpr = EAcc exprCode arrVar arrayLengthIdent
                               whileGuard = ERel exprCode idxVar (LTH exprCode) lenExpr
                               arrAccess = EIdx exprCode arrVar idxVar
                               elemDecl = Decl tCode t' [Init tCode i arrAccess]
                               idxIncr = Incr exprCode forIndexIdent
{- {                      -}   in BStmt stmtCode $ Block stmtCode [
{-  var ~l_arr = <expr>; -}            arrDecl,
{-  int ~l_idx = 0;      -}            idxDecl,
{-  while(~l_idx < ~l_arr.length)-}    While stmtCode whileGuard $
{-  {                         -}       BStmt stmtCode $ Block stmtCode [
{-   <t> <i> = ~l_arr[~l_idx];-}               elemDecl,
{-   <stmt>                   -}               stmt',
{-   ~l_idx++;                -}               idxIncr
{-  }                         -}           ]
{- }                      -}           ]
    SExp _ expr       -> SExp code (rewriteExpr expr)

-- Wrap a single statement into a block.
blkWrap :: Stmt Code -> Stmt Code
blkWrap stmt = case stmt of
    BStmt {} -> stmt
    _        -> let code = unwrap stmt
                in BStmt code (Block code [stmt])

rewriteItem :: Item Pos -> Item Code
rewriteItem item =
    let code = toCode item
    in case item of
        NoInit _ i    -> NoInit code i
        Init _ i expr -> Init code i (rewriteExpr expr)

rewriteExpr :: Expr Pos -> Expr Code
rewriteExpr expr =
    let code = toCode expr
        expr' = case expr of
            EVar _ i         -> EVar code i
            ELitInt _ n      -> ELitInt code n
            EString _ s      -> EString code s
            ELitTrue _       -> ELitTrue code
            ELitFalse _      -> ELitFalse code
            ENullI _ i       -> ENull code (Cl code i)
            ENullArr _ i     -> ENull code (Arr code (Cl code i))
            ENull _ type_    -> ENull code (rewriteType type_)
            ENew _ t         -> ENew code (rewriteType t)
            ENewArr _ t expr -> ENewArr code (rewriteType t) (rewriteExpr expr)
            EApp _ expr exprs -> EApp code (rewriteExpr expr) (map rewriteExpr exprs)
            EIdx _ expr1 expr2 -> EIdx code (rewriteExpr expr1) (rewriteExpr expr2)
            EAcc _ expr i    -> EAcc code (rewriteExpr expr) i
            ENeg _ expr      -> ENeg code (rewriteExpr expr)
            ENot _ expr      -> ENot code (rewriteExpr expr)
            EMul _ expr1 op expr2 -> EMul code (rewriteExpr expr1) (nullRewrite op) (rewriteExpr expr2)
            EAdd _ expr1 op expr2 -> EAdd code (rewriteExpr expr1) (nullRewrite op) (rewriteExpr expr2)
            ERel _ expr1 op expr2 -> ERel code (rewriteExpr expr1) (nullRewrite op) (rewriteExpr expr2)
            EAnd _ expr1 expr2 -> EAnd code (rewriteExpr expr1) (rewriteExpr expr2)
            EOr _ expr1 expr2 -> EOr code (rewriteExpr expr1) (rewriteExpr expr2)
    in  calcConstexpr expr'

-- Calculate the value of an expression containing only constants or trivial cases as leaves.
-- If the values are of incorrect types, for example someone tries to negate a string,
-- the expression remains the same and the error will be caught during the analysis phase.
calcConstexpr :: Expr Code -> Expr Code
calcConstexpr srcExpr = case srcExpr of
    ENeg code expr -> case expr of
        ELitInt _ n -> ELitInt code (-n)
        _           -> srcExpr
    ENot code expr -> case expr of
        ELitTrue _  -> ELitFalse code
        ELitFalse _ -> ELitTrue code
        _           -> srcExpr
    EMul code expr1 op expr2 -> case (expr1, expr2) of
        (ELitInt _ n1, ELitInt _ n2) -> case op of
            Times _ -> ELitInt code (n1 * n2)
            Div _   -> ELitInt code (n1 `div` n2)
            Mod _   -> ELitInt code (n1 `mod` n2)
        _                            -> srcExpr
    EAdd code expr1 op expr2 -> case (expr1, expr2) of
        (ELitInt _ n1, ELitInt _ n2) -> case op of
            Plus _  -> ELitInt code (n1 + n2)
            Minus _ -> ELitInt code (n1 - n2)
        (EString _ s1, EString _ s2) -> case op of
            Plus _ -> EString code (s1 ++ s2)
            _      -> srcExpr
        _                            -> srcExpr
    ERel code expr1 op expr2 -> case (expr1, expr2) of
        (EVar _ i1, EVar _ i2) | i1 == i2 -> case op of
            -- Comparing a variable to itself always yields equality.
            LE _  -> ELitTrue code
            GE _  -> ELitTrue code
            EQU _ -> ELitTrue code
            LTH _ -> ELitFalse code
            GTH _ -> ELitFalse code
            NE _  -> ELitFalse code
        (ELitInt _ n1, ELitInt _ n2) -> ordWithOp op n1 n2 code
        (EString _ s1, EString _ s2) -> ordWithOp op s1 s2 code
        (ELitTrue _, ELitTrue _)     -> ordWithOp op True True code
        (ELitFalse _, ELitTrue _)    -> ordWithOp op False True code
        (ELitTrue _, ELitFalse _)    -> ordWithOp op True False code
        (ELitFalse _, ELitFalse _)   -> ordWithOp op False False code
        (ENull {}, ENull {}) -> case op of
            EQU _ -> ELitTrue code
            NE _  -> ELitFalse code
        _                        -> srcExpr
    EAnd code expr1 expr2 -> case (expr1, expr2) of
        (ELitTrue _, ELitTrue _)   -> ELitTrue code
        (ELitFalse _, ELitTrue _)  -> ELitFalse code
        (ELitTrue _, ELitFalse _)  -> ELitFalse code
        (ELitFalse _, ELitFalse _) -> ELitFalse code
        _                          -> srcExpr
    EOr code expr1 expr2 -> case (expr1, expr2) of
        (ELitTrue _, ELitTrue _)   -> ELitTrue code
        (ELitFalse _, ELitTrue _)  -> ELitTrue code
        (ELitTrue _, ELitFalse _)  -> ELitTrue code
        (ELitFalse _, ELitFalse _) -> ELitFalse code
        _                          -> srcExpr
    _ -> srcExpr

-- Convert a relational operation on two values to a boolean literal.
ordWithOp :: Ord b => RelOp a -> b -> b -> Code -> Expr Code
ordWithOp op a1 a2 code =
    let b = case op of
              LTH _ -> a1 < a2
              LE _  -> a1 <= a2
              GTH _ -> a1 > a2
              GE _  -> a1 >= a2
              EQU _ -> a1 == a2
              NE _  -> a1 /= a2
    in if b then ELitTrue code else ELitFalse code

rewriteType :: Type Pos -> Type Code
rewriteType = nullRewrite

rewriteArg :: Arg Pos -> Arg Code
rewriteArg = nullRewrite

nullRewrite :: (Functor f, Unwrappable f, Positioned a, Print (f a)) => f a -> f Code
nullRewrite x = toCode x <$ x
