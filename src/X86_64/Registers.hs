module X86_64.Registers where

import qualified Data.Map            as Map
import           Espresso.Syntax.Abs

data RegType = CallerSaved | CalleeSaved deriving Eq
data Reg = Reg {reg64 :: String, reg32 :: String, reg16 :: String, reg8 :: String, regType :: RegType}
data RegState = RegS {reg :: Reg, regReserved :: Bool, regVals :: [ValIdent]} deriving Show

data RegRank = Free RegType | Clean Int | Dirty Int deriving Eq

instance Show Reg where
    show = reg64

instance Eq Reg where
    r1 == r2 = reg64 r1 == reg64 r2

instance Ord Reg where
    compare r1 r2 = compare (reg64 r1) (reg64 r2)

instance Ord RegType where
    compare rt1 rt2 = case (rt1, rt2) of
        (CallerSaved, CallerSaved) -> EQ
        (CalleeSaved, CalleeSaved) -> EQ
        (CallerSaved, CalleeSaved) -> LT
        (CalleeSaved, CallerSaved) -> GT

instance Ord RegRank where
    compare r1 r2 = case (r1, r2) of
        (Free rt1, Free rt2)   -> compare rt1 rt2
        (Clean n1, Clean n2)   -> compare n2 n1
        (Dirty n1, Dirty n2)   -> compare n2 n1
        (Free CalleeSaved, _)  -> LT
        (_, Free CalleeSaved ) -> GT
        (Free CallerSaved, _)  -> LT
        (_, Free CallerSaved ) -> GT
        (Clean c, Dirty d)     -> if d < c then GT else LT
        (Dirty d, Clean c)     -> if d < c then LT else GT

emptyState :: Reg -> RegState
emptyState r = RegS r False []

reservedState :: Reg -> RegState
reservedState r = RegS r True []

initialRegs :: Map.Map Reg RegState
initialRegs = Map.fromList [
        (rax, emptyState rax),
        (rdx, emptyState rdx),
        (rbx, emptyState rbx),
        (rcx, emptyState rcx),
        (rsi, emptyState rsi),
        (rdi, emptyState rdi),
        (rsp, reservedState rsp),
        (rbp, reservedState rbp),
        (r8, emptyState r8),
        (r9, emptyState r9),
        (r10, emptyState r10),
        (r11, emptyState r11),
        (r12, emptyState r12),
        (r13, emptyState r13),
        (r14, emptyState r14),
        (r15, emptyState r15)
    ]

rax :: Reg
rax = Reg "rax" "eax" "ax" "al" CallerSaved

rdx :: Reg
rdx = Reg "rdx" "edx" "dx" "dl" CallerSaved

rbx :: Reg
rbx = Reg "rbx" "ebx" "bx" "bl" CalleeSaved

rcx :: Reg
rcx = Reg "rcx" "ecx" "cx" "cl" CallerSaved

rsi :: Reg
rsi = Reg "rsi" "esi" "si" "sil" CallerSaved

rdi :: Reg
rdi = Reg "rdi" "edi" "di" "dil" CallerSaved

rsp :: Reg
rsp = Reg "rsp" "esp" "sp" "spl" CallerSaved

rbp :: Reg
rbp = Reg "rbp" "ebp" "bp" "bpl" CalleeSaved

r8 :: Reg
r8 = Reg "r8" "r8d" "r8w" "r8b" CallerSaved

r9 :: Reg
r9 = Reg "r9" "r9d" "r9w" "r9b" CallerSaved

r10 :: Reg
r10 = Reg "r10" "r10d" "r10w" "r10b" CallerSaved

r11 :: Reg
r11 = Reg "r11" "r11d" "r11w" "r11b" CallerSaved

r12 :: Reg
r12 = Reg "r12" "r12d" "r12w" "r12b" CalleeSaved

r13 :: Reg
r13 = Reg "r13" "r13d" "r13w" "r13b" CalleeSaved

r14 :: Reg
r14 = Reg "r14" "r14d" "r14w" "r14b" CalleeSaved

r15 :: Reg
r15 = Reg "r15" "r15d" "r15w" "r15b" CalleeSaved

argReg :: Integer -> Maybe Reg
argReg idx = case idx of
    0 -> Just rdi
    1 -> Just rsi
    2 -> Just rdx
    3 -> Just rcx
    4 -> Just r8
    5 -> Just r9
    _ -> Nothing
