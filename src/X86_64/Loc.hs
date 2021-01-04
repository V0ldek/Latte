module X86_64.Loc where

import           Data.Int
import           X86_64.Registers

data Loc = LocImm Int32 | LocConst String | LocReg Reg | LocStack Int  deriving (Eq, Show)

-- This is the same as deriving, but we rely on this ordering so it is stated explicitly.
-- When extracting a value from a location we want the cheapest possibilities first,
-- so it's immediate values, registers and then memory.
instance Ord Loc where
    compare l1 l2 = case (l1, l2) of
        (LocReg r1, LocReg r2)     -> compare r1 r2
        (LocStack n1, LocStack n2) -> compare n1 n2
        (LocConst s1, LocConst s2) -> compare s1 s2
        (LocImm n1, LocImm n2)     -> compare n1 n2
        (LocImm _, _)              -> LT
        (_, LocImm _)              -> GT
        (LocConst _, _)            -> LT
        ( _, LocConst _)           -> GT
        (LocReg _, _)              -> LT
        (_, LocReg _)              -> GT

isStack :: Loc -> Bool
isStack = not . isNonStack

isNonStack :: Loc -> Bool
isNonStack loc = case loc of
    LocStack _ -> False
    _          -> True

isReg :: Loc -> Bool
isReg loc = case loc of
    LocReg _ -> True
    _        -> False

asReg :: Loc -> Reg
asReg loc = case loc of
    LocReg r -> r
    _        -> error "asReg: not a reg"
