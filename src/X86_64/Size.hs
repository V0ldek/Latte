-- Sizes of different types of values from Espresso.
module X86_64.Size where

import           Data.Int
import           Espresso.Syntax.Abs

data Size = Byte | Double | Quadruple deriving (Eq, Show)

sizeInBytes :: Size -> Int64
sizeInBytes size = case size of
    Byte      -> 1
    Double    -> 4
    Quadruple -> 8

typeSize :: SType a -> Size
typeSize t = case t of
    Int _   -> Double
    Bool _  -> Byte
    Ref _ _ -> Quadruple
    _       -> error "typeSize: invalid type"

valSize :: Val a -> Size
valSize val = case val of
    VInt _  _    -> Double
    VNegInt _  _ -> Double
    VTrue _      -> Byte
    VFalse _     -> Byte
    VNull _      -> Quadruple
    VVal _ t _   -> typeSize t
