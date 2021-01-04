module X86_64.Size where

import           Espresso.Syntax.Abs

data Size = Byte | Double | Quadruple deriving Eq

sizeInBytes :: Size -> Int
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
