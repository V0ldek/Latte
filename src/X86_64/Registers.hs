module X86_64.Registers where

import qualified Data.Map            as Map
import           Espresso.Syntax.Abs

data RegType = CallerSaved | CalleeSaved deriving Eq
data Reg = Reg {regHigh :: String, regLow :: String, regType :: RegType}
data RegState = RegS {reg :: Reg, regReserved :: Bool, regVals :: [ValIdent]}

data RegRank = Free RegType | Clean Int | Dirty Int deriving Eq

instance Show Reg where
    show = regHigh

instance Eq Reg where
    r1 == r2 = regHigh r1 == regHigh r2

instance Ord Reg where
    compare r1 r2 = compare (regHigh r1) (regHigh r2)

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
rax = Reg "rax" "eax" CallerSaved

rdx :: Reg
rdx = Reg "rdx" "edx" CallerSaved

rbx :: Reg
rbx = Reg "rbx" "ebx" CalleeSaved

rcx :: Reg
rcx = Reg "rcx" "ecx" CallerSaved

rsi :: Reg
rsi = Reg "rsi" "esi" CallerSaved

rdi :: Reg
rdi = Reg "rdi" "edi" CallerSaved

rsp :: Reg
rsp = Reg "rsp" "esp" CallerSaved

rbp :: Reg
rbp = Reg "rbp" "ebp" CalleeSaved

r8 :: Reg
r8 = Reg "r8" "r8d" CallerSaved

r9 :: Reg
r9 = Reg "r9" "r9d" CallerSaved

r10 :: Reg
r10 = Reg "r10" "r10d" CallerSaved

r11 :: Reg
r11 = Reg "r11" "r11d" CallerSaved

r12 :: Reg
r12 = Reg "r12" "r12d" CalleeSaved

r13 :: Reg
r13 = Reg "r13" "r13d" CalleeSaved

r14 :: Reg
r14 = Reg "r14" "r14d" CalleeSaved

r15 :: Reg
r15 = Reg "r15" "r15d" CalleeSaved

argReg :: Integer -> Maybe Reg
argReg idx = case idx of
    0 -> Just rdi
    1 -> Just rsi
    2 -> Just rdx
    3 -> Just rcx
    4 -> Just r8
    5 -> Just r9
    _ -> Nothing
