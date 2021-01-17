{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE FlexibleInstances #-}
module Syntax.Abs where

import           Data.Maybe (fromJust)

-- Haskell module generated by the BNF converter

type Pos = (Int, Int)
newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program a = Program a [TopDef a]
  deriving (Eq, Ord, Show, Read, Foldable)

showI :: Ident -> String
showI (Ident i) = i

class Positioned a where
  pos :: a -> Maybe Pos

class Unwrappable f where
  unwrap :: f a -> a

unwrapPos :: Program (Maybe Pos) -> Program Pos
unwrapPos p = fromJust <$> p

instance Positioned Pos where
  pos = Just

instance Functor Program where
    fmap f x = case x of
        Program a topdefs -> Program (f a) (map (fmap f) topdefs)

instance Unwrappable Program where
  unwrap x = case x of
    Program a _ -> a

data TopDef a
    = FnDef a (Type a) Ident [Arg a] (Block a)
    | ClDef a Ident (ClBlock a)
    | ClExtDef a Ident Ident (ClBlock a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor TopDef where
    fmap f x = case x of
        FnDef a type_ ident args block -> FnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
        ClDef a ident clblock -> ClDef (f a) ident (fmap f clblock)
        ClExtDef a ident1 ident2 clblock -> ClExtDef (f a) ident1 ident2 (fmap f clblock)

instance Unwrappable TopDef where
  unwrap x = case x of
    FnDef a _ _ _ _  -> a
    ClDef a _ _      -> a
    ClExtDef a _ _ _ -> a

data Arg a = Arg a (Type a) Ident
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Arg where
    fmap f x = case x of
        Arg a type_ ident -> Arg (f a) (fmap f type_) ident

instance Unwrappable Arg where
  unwrap x = case x of
    Arg a _ _  -> a

data ClBlock a = ClBlock a [ClDef a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor ClBlock where
    fmap f x = case x of
        ClBlock a cldefs -> ClBlock (f a) (map (fmap f) cldefs)

instance Unwrappable ClBlock where
  unwrap x = case x of
    ClBlock a _  -> a

data ClDef a
    = MthDef a (Type a) Ident [Arg a] (Block a)
    | FldDef a (Type a) Ident
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor ClDef where
    fmap f x = case x of
        MthDef a type_ ident args block -> MthDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
        FldDef a type_ ident -> FldDef (f a) (fmap f type_) ident

instance Unwrappable ClDef where
  unwrap x = case x of
    MthDef a _ _ _ _ -> a
    FldDef a _ _     -> a

data Block a = Block a [Stmt a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Block where
    fmap f x = case x of
        Block a stmts -> Block (f a) (map (fmap f) stmts)

instance Unwrappable Block where
  unwrap x = case x of
    Block a _ -> a

data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a (Expr a) (Expr a)
    | Incr a Ident
    | Decr a Ident
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | For a (Type a) Ident (Expr a) (Stmt a)
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Decl a type_ items -> Decl (f a) (fmap f type_) (map (fmap f) items)
        Ass a expr1 expr2 -> Ass (f a) (fmap f expr1) (fmap f expr2)
        Incr a ident -> Incr (f a) ident
        Decr a ident -> Decr (f a) ident
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        For a type_ ident expr stmt -> For (f a) (fmap f type_) ident (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)

instance Unwrappable Stmt where
  unwrap x = case x of
    Empty a          -> a
    BStmt a _        -> a
    Decl a _ _       -> a
    Ass a _ _        -> a
    Incr a _         -> a
    Decr a _         -> a
    Ret a _          -> a
    VRet a           -> a
    Cond a _ _       -> a
    CondElse a _ _ _ -> a
    While a _ _      -> a
    For a _ _ _ _    -> a
    SExp a _         -> a

data Item a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Item where
    fmap f x = case x of
        NoInit a ident    -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)

instance Unwrappable Item where
  unwrap x = case x of
    NoInit a _ -> a
    Init a _ _ -> a

data Type a
    = Int a
    | Bool a
    | Void a
    | Var a
    | Arr a (Type a)
    | Cl a Ident
    | Fun a (Type a) [Type a]
    | Ref a (Type a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Type where
    fmap f x = case x of
        Int a             -> Int (f a)
        Bool a            -> Bool (f a)
        Void a            -> Void (f a)
        Var a             -> Var (f a)
        Arr a type_       -> Arr (f a) (fmap f type_)
        Cl a ident        -> Cl (f a) ident
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)
        Ref a type_       -> Ref (f a) (fmap f type_)

instance Unwrappable Type where
    unwrap x = case x of
      Int a     -> a
      Bool a    -> a
      Void a    -> a
      Var a     -> a
      Arr a _   -> a
      Cl a _    -> a
      Fun a _ _ -> a
      Ref a _   -> a

data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | EString a String
    | ELitTrue a
    | ELitFalse a
    | ENullI a Ident
    | ENullArr a Ident
    | ENull a (Type a)
    | ENew a (Type a)
    | ENewArr a (Type a) (Expr a)
    | EApp a (Expr a) [Expr a]
    | EIdx a (Expr a) (Expr a)
    | EAcc a (Expr a) Ident
    | ENeg a (Expr a)
    | ENot a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        EString a string -> EString (f a) string
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        ENullI a ident -> ENullI (f a) ident
        ENullArr a ident -> ENullArr (f a) ident
        ENull a type_ -> ENull (f a) (fmap f type_)
        ENew a type_ -> ENew (f a) (fmap f type_)
        ENewArr a type_ expr -> ENewArr (f a) (fmap f type_) (fmap f expr)
        EApp a expr exprs -> EApp (f a) (fmap f expr) (map (fmap f) exprs)
        EIdx a expr1 expr2 -> EIdx (f a) (fmap f expr1) (fmap f expr2)
        EAcc a expr ident -> EAcc (f a) (fmap f expr) ident
        ENeg a expr -> ENeg (f a) (fmap f expr)
        ENot a expr -> ENot (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)

instance Unwrappable Expr where
  unwrap x = case x of
    EVar a _      -> a
    ELitInt a _   -> a
    EString a _   -> a
    ELitTrue a    -> a
    ELitFalse a   -> a
    ENullI a _    -> a
    ENullArr a _  -> a
    ENull a _     -> a
    ENew a _      -> a
    ENewArr a _ _ -> a
    EApp a _ _    -> a
    EIdx a _ _    -> a
    EAcc a _ _    -> a
    ENeg a _      -> a
    ENot a _      -> a
    EMul a _ _ _  -> a
    EAdd a _ _ _  -> a
    ERel a _ _ _  -> a
    EAnd a _ _    -> a
    EOr a _ _     -> a

data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor AddOp where
    fmap f x = case x of
        Plus a  -> Plus (f a)
        Minus a -> Minus (f a)

instance Unwrappable AddOp where
  unwrap x = case x of
    Plus a  -> a
    Minus a -> a

data MulOp a = Times a | Div a | Mod a
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a   -> Div (f a)
        Mod a   -> Mod (f a)

instance Unwrappable MulOp where
  unwrap x = case x of
    Times a -> a
    Div a   -> a
    Mod a   -> a

data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a  -> LE (f a)
        GTH a -> GTH (f a)
        GE a  -> GE (f a)
        EQU a -> EQU (f a)
        NE a  -> NE (f a)

instance Unwrappable RelOp where
  unwrap x = case x of
    LTH a -> a
    LE a  -> a
    GTH a -> a
    GE a  -> a
    EQU a -> a
    NE a  -> a
