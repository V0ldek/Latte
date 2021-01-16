-- Instruction set used by the codegen and functions to emit them.
-- Uses AT&T syntax.
module X86_64.CodeGen.Emit (
    EmitM(..),
    emitAsString,
    add,
    and,
    callAddress,
    callDirect,
    cdq,
    cmp,
    commentMultiline,
    constDef,
    decrStack,
    extern,
    globalMain,
    idiv,
    imul,
    incrStack,
    jmp,
    jz,
    label,
    lea,
    leaOfConst,
    leave,
    movConst,
    movFromMemToReg,
    movFromRegToMem,
    movToReg,
    movToStack,
    neg,
    pop,
    push,
    quadDef,
    ret,
    sal,
    sar,
    sete,
    setg,
    setge,
    setl,
    setle,
    setne,
    sub,
    test,
    xor,
) where

import           Data.Int
import           Espresso.Syntax.Abs
import           Identifiers
import           Prelude               hiding (and)
import           X86_64.CodeGen.Consts
import           X86_64.Loc
import           X86_64.Registers      hiding (reg)
import           X86_64.Size

class EmitM m where
    -- Emit a single instruction.
    emit :: String -> m ()

newtype PhonyEmit a = PE String

instance EmitM PhonyEmit where
    emit = PE

emitAsString :: PhonyEmit () -> String
emitAsString f = let PE s = f in s

-- Emit an addition operation between a source and destination location.
-- Saves the result in the destination.
--   add<s> <src>, <dest> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
add :: EmitM m => Loc -> Loc -> String -> m ()
add src dest comment_ =
    let srcString = loc Double src
        destString = loc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. add to immediate"
        (LocStack _, LocStack _) -> error "internal error. add from stack to stack"
        _ -> emitInd $ bin "add" Double srcString destString comment_

-- Emit an and operation between a source and destination location.
-- Saves the result in the destination.
--   and<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
and :: EmitM m => Size -> Loc -> Loc -> String -> m ()
and size src dest comment_ =
    let srcString = loc size src
        destString = loc size dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. and to immediate"
        (LocStack _, LocStack _) -> error "internal error. and from stack to stack"
        _ -> emitInd $ bin "and" size srcString destString comment_

-- Emit a call instruction to the address in the given memory location.
--   call *<offset>(%<reg>) # <comment>
callAddress :: EmitM m => Reg -> Int64 -> String -> m ()
callAddress reg_ offset comment_ =
    emitInd $ "call *" ++ show offset ++ "(" ++ sizedReg Quadruple reg_ ++ ")" ++ comment comment_

-- Emit a call instruction to a label.
--   call <f>
callDirect :: EmitM m => String -> m ()
callDirect f = emitInd $ "call " ++ sanitiseAssembly f

-- Emit a sign-extend instruction for division purposes, see idiv.
-- Loads the sign of eax into edx.
--   cdq
cdq :: EmitM m => m ()
cdq = emitInd "cdq"

-- Emit a comparison between two locations, where the first location
-- is logically the right-hand-side of the comparison.
--   cmp<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
cmp :: EmitM m => Size -> Loc -> Loc -> m ()
cmp size rhs lhs =
    let rhsString = loc size rhs
        lhsString = loc size lhs
    in case (rhs, lhs) of
        (LocImm _, LocImm _) -> error "internal error. cmp on immediates"
        (LocStack _, LocStack _) -> error "internal error. cmp on two stack locs"
        _ -> emitInd $ bin "cmp" size rhsString lhsString ""

-- Emit a label to a compile-time constant string.
constDef :: EmitM m => Const -> m ()
constDef c = emit $ constName c ++ ":\n  .string " ++ show (constValue c)

-- Emit an instruction logically decreasing the stack
-- by increasing the rsp pointer.
--   subq $<n>, %rsp # <comment>
decrStack :: EmitM m => Int64 -> m ()
decrStack n = emitInd $ "addq " ++ lit64 n ++ ", %rsp"

-- Emit a declaration of an external call target.
extern :: EmitM m => String -> m ()
extern s = emit $ ".extern " ++ s

-- Emit a declaration of the main function as entry point.
globalMain :: EmitM m => m ()
globalMain = emit ".global main"

-- Emit an instruction logically increasing the stack
-- by decreasing the rsp pointer.
--   subq $<n>, %rsp # <comment>
incrStack :: EmitM m => Int64 -> String -> m ()
incrStack n comment_ = emitInd $ "subq " ++ lit64 n ++ ", %rsp" ++ comment comment_

-- Emit a division instruction.
-- The left-hand-side has to be loaded to the eax register
-- and sign-extended to edx manually or by using the cdq instruction.
-- The other location must not be an immediate.
-- The division is stored in eax and the remainder in edx.
--   idiv<s> <loc>
-- where <s> is the AT&T instruction suffix based on <size>.
idiv :: EmitM m => Size -> Loc -> m ()
idiv size loc_ = case loc_ of
    LocImm {} -> error "internal error. idiv on an immediate."
    LocPtr {} -> error "internal error. idiv with ptr"
    _         -> emitInd $ "idiv" ++ sizeSuf size ++ " " ++ loc size loc_

-- Emit a multiplication operation between a source and destination location.
--   imul<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
imul :: EmitM m => Loc -> Loc -> m ()
imul src dest =
    let srcString = loc Double src
        destString = loc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. mul to immediate"
        (LocStack _, LocStack _) -> error "internal error. mul from stack to stack"
        _ -> emitInd $ bin "imul" Double srcString destString ""

-- Emit a jump to a label.
--   jmp <l>
jmp :: EmitM m => LabIdent -> m ()
jmp (LabIdent l) = emitInd $ "jmp " ++ sanitiseAssembly l

-- Emit a conditional jump instruction that tests the ZF CPU flag
-- and jumps if it is set.
--   jz <l>
jz :: EmitM m => LabIdent -> m ()
jz (LabIdent l) = emitInd $ "jz " ++ sanitiseAssembly l

-- Emit a label.
-- <l>: # <comment>
label :: EmitM m => LabIdent -> String -> m ()
label (LabIdent l) comment_ = emit $ sanitiseAssembly l ++ ":" ++ comment comment_


lea :: EmitM m => Reg -> Int64 -> Reg -> Size -> Loc -> String -> m ()
lea baseReg offset idxReg scale dest comment_ =
    emitInd $ "lea " ++ complexPtr baseReg offset idxReg scale ++ ", " ++ loc Quadruple dest ++ comment comment_

-- Emit an address load operation for a compile-time constant.
--   lea <x>(%rip), %<reg>
-- where <s> is the AT&T instruction suffix based on <size>.
leaOfConst :: EmitM m => String -> Reg -> m ()
leaOfConst x dest =
    emitInd $ "lea " ++ sanitiseAssembly x ++ "(%rip), " ++ reg (reg64 dest)

-- Emit a standard epilogue leave instruction
-- that restores the rsp and rbp registers.
leave :: EmitM m => m ()
leave = emitInd "leave"

-- Move the pointer to a constant into a location.
--   movq $<i>, <loc>
movConst :: EmitM m => LabIdent -> Reg -> m ()
movConst (LabIdent i) reg_ = emitInd $ bin "mov" Quadruple (i ++ "(%rip)") (sizedReg Quadruple reg_) ""

-- Emit a move from a source location to a register.
--   mov<s> <src>, %<dest> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movToReg :: EmitM m => Size -> Loc -> Reg -> String -> m ()
movToReg size src dest comment_ =
    let srcString = loc size src
    in  emitInd $ bin "mov" size srcString (sizedReg size dest) comment_

-- Emit a move from a source location to a stack destination.
--   mov<s> <src>, <stackDest>(%rbp) # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movToStack :: EmitM m => Size -> Loc -> Int64 -> String -> m ()
movToStack size src stackDest comment_ = case src of
    LocReg reg_   -> emitInd $ bin "mov" size (sizedReg size reg_) (stack stackDest) comment_
    LocPtr src' _ -> movToStack size src' stackDest comment_
    LocImm int    -> emitInd $ bin "mov" size (lit32 int) (stack stackDest) comment_
    LocImm64 int  -> emitInd $ bin "mov" size (lit64 int) (stack stackDest) comment_
    LocStack _    -> error "internal error. mov from stack to stack"

-- Emit a move from a memory location pointed to by the value in the `ptrReg`
-- offset by `ptrOffset` bytes to the destination register.
--  mov<s> <ptrOffset>(%<ptrReg>), %<destReg> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movFromMemToReg :: EmitM m => Size -> Reg -> Int64 -> Reg -> String -> m ()
movFromMemToReg size ptrReg ptrOffset destReg comment_ =
    emitInd $ bin "mov" size (ptr ptrReg ptrOffset) (sizedReg size destReg) comment_

-- Emit a move from a register to the memory location pointed to by the
-- value in the `ptrReg` offset by `ptrOffset` bytes.
--   mov<s> %<srcReg>, <ptrOffset>(%<ptrReg>)
-- where <s> is the AT&T instruction suffix based on <size>.
movFromRegToMem :: EmitM m => Size -> Reg -> Reg -> Int64 -> m ()
movFromRegToMem size srcReg ptrReg ptrOffset =
    emitInd $ bin "mov" size (sizedReg size srcReg) (ptr ptrReg ptrOffset) ""

-- Emit a negation operation on a register.
--   neg %<reg>
neg :: EmitM m => Reg -> m ()
neg reg_ = emitInd $ "neg" ++ sizeSuf Double ++ " " ++ sizedReg Double reg_

-- Emit a pop instruction that pops the top of the stack into the location.
-- The size of the pop is always 8 bytes.
--   pop <loc>
pop :: EmitM m => Loc -> m ()
pop srcloc = emitInd $ "pop " ++ loc Quadruple srcloc

-- Emit a push instruction that pushes contents of the location on the stack.
-- The size of the push is always 8 bytes.
--   push <loc>
push :: EmitM m => Loc -> String -> m ()
push srcloc comment_ = emitInd $ "push " ++ loc Quadruple srcloc ++ comment comment_

-- Emit a definition of a quad value.
--   .quad <s>
quadDef :: EmitM m => String -> m ()
quadDef s = emitInd $ ".quad " ++ sanitiseAssembly s

-- Emit a ret instruction that ends the current function call.
ret :: EmitM m => m ()
ret = emitInd "ret"

-- Emit an instruction that shifts a register bitwise to the left by a given offset.
-- Logically this is a multiply-by-2^n operation.
--   sall $<n>, %<reg>
sal :: EmitM m => Int -> Reg -> String -> m ()
sal n loc_ comment_ = emitInd $ "sal " ++ lit n ++ ", " ++ sizedReg Double loc_ ++ comment comment_

-- Emit an instruction that shifts a register bitwise to the right by a given offset.
-- Logically this is a divide-by-2^n operation.
--   sarl $<n>, %<reg>
sar :: EmitM m => Int -> Reg -> String -> m ()
sar n loc_ comment_ = emitInd $ "sar " ++ lit n ++ ", " ++ sizedReg Double  loc_ ++ comment comment_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as an
-- equal-to comparison.
--   sete %<reg>
sete :: EmitM m => Reg -> m ()
sete reg_ = emitInd $ "sete " ++ sizedReg Byte reg_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- greater-than comparison.
--   setg %<reg>
setg :: EmitM m => Reg -> m ()
setg reg_ = emitInd $ "setg " ++ sizedReg Byte reg_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- greater-than-or-equal-to comparison.
--   setge %<reg>
setge :: EmitM m => Reg -> m ()
setge reg_ = emitInd $ "setge " ++ sizedReg Byte reg_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- less-than comparison.
--   setl %<reg>
setl :: EmitM m => Reg -> m ()
setl reg_ = emitInd $ "setl " ++ sizedReg Byte reg_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- less-than-or-equal-to comparison.
--   setle %<reg>
setle :: EmitM m => Reg -> m ()
setle reg_ = emitInd $ "setle " ++ sizedReg Byte reg_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- not-equal-to comparison.
--   setne %<reg>
setne :: EmitM m => Reg -> m ()
setne reg_ = emitInd $ "setne " ++ sizedReg Byte reg_

-- Emit a subtraction operation between a source and destination location (dest - src).
--   sub<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
sub :: EmitM m => Loc -> Loc -> m ()
sub src dest =
    let srcString = loc Double src
        destString = loc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. sub to immediate"
        (LocStack _, LocStack _) -> error "internal error. sub from stack to stack"
        _ -> emitInd $ bin "sub" Double srcString destString ""

-- Emit a test instruction between two locations that sets CPU flags
-- based on the result of a bitwise-and performed on the operands.
-- The operands are considered for their lower 8 bytes only.
--   testb <op1>, <op2>
test :: EmitM m => Size -> Loc -> Loc -> m ()
test size op1 op2 =
    let op1String = loc size op1
        op2String = loc size op2
    in emitInd $ bin "test" size op1String op2String ""

-- Emit a xor operation between a source and destination location.
-- Saves the result in the destination.
--   xor<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
xor :: EmitM m => Size -> Loc -> Loc -> m ()
xor size src dest =
    let srcString = loc size src
        destString = loc size dest
    in case (src, dest) of
        (LocImm _, LocImm _) -> error "internal error. xor on immediates"
        (LocStack _, LocStack _) -> error "internal error. xor on two stack locs"
        _ -> emitInd $ bin "xor" size srcString destString ""

-- String representation of a binary instruction with a given size suffix,
-- two operands and an end-of-line comment.
bin :: String -> Size -> String -> String -> String -> String
bin instr size x y comment_ = instr ++ sizeSuf size ++ " " ++ x ++ ", " ++ y ++ comment comment_

-- String representation of an end-of-line comment
comment :: String -> String
comment [] = []
comment s  = ' ':'#':' ':s

commentMultiline :: EmitM m => [String] -> m ()
commentMultiline = emit . unlines . map comment

complexPtr :: Reg -> Int64 -> Reg -> Size -> String
complexPtr baseReg offset idxReg scale =
    show offset ++ "("
        ++ sizedReg Quadruple baseReg ++ ","
        ++ sizedReg Quadruple idxReg ++ ","
        ++ show (sizeInBytes scale) ++ ")"

-- Emit an instruction indented by 2 spaces.
emitInd :: EmitM m => String -> m ()
emitInd s = emit ("  " ++ s)

-- String representation of an integral literal.
lit :: Int -> String
lit n = '$':show n

-- String representation of a memory access,
-- where the base is located in the given register and is offset
-- by the passed offset.
ptr :: Reg -> Int64 -> String
ptr r offset = show offset ++ "(" ++ sizedReg Quadruple r ++ ")"

-- String representation of an integral literal.
lit32 :: Int32 -> String
lit32 n = '$':show n

-- String representation of an integral literal.
lit64 :: Int64 -> String
lit64 n = '$':show n

-- String representation of a location.
loc :: Size -> Loc -> String
loc size loc_ = case loc_ of
    LocReg r   -> sizedReg size r
    LocPtr l _ -> loc size l
    LocStack n -> stack n
    LocImm n   -> lit32 n
    LocImm64 n -> lit64 n

-- String representation of a register (full 64-bits).
reg :: String -> String
reg r = '%':r

-- String representation of a register identifier for a given size.
sizedReg :: Size -> Reg -> String
sizedReg size r = case size of
    Byte      -> reg $ reg8 r
    Double    -> reg $ reg32 r
    Quadruple -> reg $ reg64 r

-- AT&T size suffix for a given operand size.
sizeSuf :: Size -> String
sizeSuf s = case s of
    Byte      -> "b"
    Double    -> "l"
    Quadruple -> "q"

-- String representation of a stack location.
stack :: Int64 -> String
stack n = show n ++ "(%rbp)"
