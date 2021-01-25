module X86_64.Registers where

data RegType = CallerSaved | CalleeSaved deriving (Eq, Show)
data Reg = Reg {
    -- Identifier for the 64 bits of the register.
    reg64   :: String,
    -- Identifier for the lower 32 bits of the register.
    reg32   :: String,
    -- Identifier for the lower 16 bits of the register.
    reg16   :: String,
    -- Identifier for the lower 8 bits of the register.
    reg8    :: String,
    -- Whether the register is caller or callee saved.
    regType :: RegType
}

instance Show Reg where
    show = reg64

instance Eq Reg where
    r1 == r2 = reg64 r1 == reg64 r2

instance Ord Reg where
    compare r1 r2 = compare (reg64 r1) (reg64 r2)

-- Caller saved registers are preferred over callee saved.
instance Ord RegType where
    compare rt1 rt2 = case (rt1, rt2) of
        (CallerSaved, CallerSaved) -> EQ
        (CalleeSaved, CalleeSaved) -> EQ
        (CalleeSaved, CallerSaved) -> LT
        (CallerSaved, CalleeSaved) -> GT

allRegs :: [Reg]
allRegs = [rax, rdx, rbx, rcx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]

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
