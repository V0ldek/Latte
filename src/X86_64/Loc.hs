-- Data type for representing variable locations during execution
-- such as register or memory.
module X86_64.Loc where

import           Data.Int
import           X86_64.Registers

data Loc = LocImm Int32 | LocReg Reg | LocStack Int64  deriving (Eq, Show)

-- This is the same as deriving(Ord), but we rely on this ordering so it is stated explicitly.
-- When extracting a value from a location we want the cheapest possibilities first,
-- so it's immediate values, registers and then memory.
instance Ord Loc where
    compare l1 l2 = case (l1, l2) of
        (LocReg r1, LocReg r2)     -> compare r1 r2
        (LocStack n1, LocStack n2) -> compare n1 n2
        (LocImm n1, LocImm n2)     -> compare n1 n2
        (LocImm _, _)              -> LT
        (_, LocImm _)              -> GT
        (LocReg _, _)              -> LT
        (_, LocReg _)              -> GT

isNonStack :: Loc -> Bool
isNonStack = not . isStack

isStack :: Loc -> Bool
isStack loc = case loc of
    LocStack _ -> True
    _          -> False

isReg :: Loc -> Bool
isReg loc = case loc of
    LocReg _ -> True
    _        -> False

asReg :: Loc -> Reg
asReg loc = case loc of
    LocReg r -> r
    _        -> error "asReg: not a reg"

-- For n-th argument of a function (starting from zero) give
-- the location it is stored in. Assumes only the return
-- address is stored after arguments on stack, the consumer
-- must correct for the actual offset.
argLoc :: Integer -> Loc
argLoc idx = case idx of
    0 -> LocReg rdi
    1 -> LocReg rsi
    2 -> LocReg rdx
    3 -> LocReg rcx
    4 -> LocReg r8
    5 -> LocReg r9
    _ -> LocStack ((fromInteger idx - 6) * 8 + 8)
