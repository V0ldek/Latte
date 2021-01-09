module Espresso.Types where

import           Espresso.Syntax.Abs
import           Espresso.Utilities  (toSymIdent)
import qualified Syntax.Abs          as Latte

defaultVal :: SType a -> Val ()
defaultVal t = case deref t of
    Int _   -> VInt () 0
    Bool _  -> VFalse ()
    Str _   -> VNull ()
    Cl _ _  -> VNull ()
    Arr _ _ -> VNull ()
    _       -> error $ "defaultVal: invalid type " ++ show (() <$ t)

deref :: SType a -> SType a
deref t = case t of
    Ref _ t' -> t'
    _        -> t

isInt :: SType a -> Bool
isInt t = case deref t of
    Int _ -> True
    _     -> False

isStr :: SType a -> Bool
isStr t = case deref t of
    Str _ -> True
    _     -> False

toSType :: Latte.Type a -> SType a
toSType t = case t of
    Latte.Int a                -> Int a
    Latte.Str a                -> Ref a (Str a)
    Latte.Bool a               -> Bool a
    Latte.Void a               -> Void a
    Latte.Var {}               -> error "toSType: not a simple type 'var'"
    Latte.Arr a t'             -> Arr a (toSType t')
    Latte.Cl a i               -> Cl a (toSymIdent i)
    Latte.Fun{}                -> error "toSType: not a simple type Fun"
    Latte.Ref _ (Latte.Int a)  -> Int a
    Latte.Ref _ (Latte.Bool a) -> Bool a
    Latte.Ref a t'             -> Ref a (deref $ toSType t')

toFType :: Latte.Type a -> FType a
toFType t = case t of
    Latte.Fun a r ps -> FType a (toSType r) (map toSType ps)
    _                -> error "not a function type"

valType :: Val a -> SType a
valType val = case val of
    VInt a _    -> Int a
    VNegInt a _ -> Int a
    VTrue a     -> Bool a
    VFalse a    -> Bool a
    VNull {}    -> error "objects unimplemented - requires grammar change to include type info with nulls"
    VVal _ t _  -> t
