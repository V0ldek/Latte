module Espresso.CodeGen.Operators where

import           Espresso.Syntax.Abs
import qualified Syntax.Abs          as Latte

toEspressoMulOp :: Latte.MulOp a -> Op ()
toEspressoMulOp op = case op of
    Latte.Times _ -> OpMul ()
    Latte.Div _   -> OpDiv ()
    Latte.Mod _   -> OpMod ()

toEspressoAddOp :: Latte.AddOp a -> Op ()
toEspressoAddOp op = case op of
    Latte.Plus _  -> OpAdd ()
    Latte.Minus _ -> OpSub ()

toEspressoRelOp :: Latte.RelOp a -> Op ()
toEspressoRelOp op = case op of
    Latte.EQU _ -> OpEQU ()
    Latte.NE _  -> OpNE ()
    Latte.GE _  -> OpGE ()
    Latte.GTH _ -> OpGTH ()
    Latte.LE _  -> OpLE ()
    Latte.LTH _ -> OpLTH ()
