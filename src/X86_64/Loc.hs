-- Data type for representing variable locations during execution
-- such as register or memory.
module X86_64.Loc where

import           Data.Int
import           X86_64.Registers
import           X86_64.Size

data Loc = LocImm Int32
         | LocImm64 Int64
         | LocReg Reg
         | LocPtr Reg Int64
         | LocPtrCmplx { ptrBase :: Reg, ptrIdx :: Reg, ptrOffset :: Int64, ptrScale :: Size}
    deriving (Eq, Show)

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
    _ -> LocPtr rbp ((fromInteger idx - 6) * 8 + 8)
