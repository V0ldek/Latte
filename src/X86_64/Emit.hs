module X86_64.Emit where

import           Data.Int
import           Espresso.Syntax.Abs
import           X86_64.Loc
import           X86_64.Registers    hiding (reg)
import           X86_64.Size

class EmitM m where
    emit :: String -> m ()

emitInd :: EmitM m => String -> m ()
emitInd s = emit ("  " ++ s)

label :: EmitM m => LabIdent -> String -> m ()
label (LabIdent l) comment = emit $ sanitise l ++ ":" ++ emitComment comment

emitComment :: String -> String
emitComment [] = []
emitComment s  = ' ':'#':' ':s

movToStack :: EmitM m => Size -> Loc -> Int64 -> String -> m ()
movToStack size src stackDest comment = case src of
    LocReg reg_ -> emitInd $ bin "mov" size (sizedReg size reg_) (stack stackDest) comment
    LocImm int  -> emitInd $ bin "mov" size (lit int) (stack stackDest) comment
    LocStack _  -> error "internal error. mov from stack to stack"

movToReg :: EmitM m => Size -> Loc -> Reg -> String -> m ()
movToReg size src dest comment =
    let srcString = emitLoc size src
    in  emitInd $ bin "mov" size srcString (sizedReg size dest) comment

jmp :: EmitM m => LabIdent -> m ()
jmp (LabIdent l) = emitInd $ "jmp " ++ sanitise l

add :: EmitM m => Loc -> Loc -> m ()
add src dest =
    let srcString = emitLoc Double src
        destString = emitLoc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. add to immediate"
        (LocStack _, LocStack _) -> error "internal error. add from stack to stack"
        _ -> emitInd $ bin "add" Double srcString destString ""

sub :: EmitM m => Loc -> Loc -> m ()
sub src dest =
    let srcString = emitLoc Double src
        destString = emitLoc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. sub to immediate"
        (LocStack _, LocStack _) -> error "internal error. sub from stack to stack"
        _ -> emitInd $ bin "sub" Double srcString destString ""

imul :: EmitM m => Loc -> Loc -> m ()
imul src dest =
    let srcString = emitLoc Double src
        destString = emitLoc Double dest
    in case (src, dest) of
        (_, LocImm _) -> error "internal error. mul to immediate"
        (LocStack _, LocStack _) -> error "internal error. mul from stack to stack"
        _ -> emitInd $ bin "imul" Double srcString destString ""

cmp :: EmitM m => Size -> Loc -> Loc -> m ()
cmp size rhs lhs =
    let rhsString = emitLoc size rhs
        lhsString = emitLoc size lhs
    in case (rhs, lhs) of
        (LocImm _, LocImm _) -> error "internal error. cmp on immediates"
        (LocStack _, LocStack _) -> error "internal error. cmp on two stack locs"
        _ -> emitInd $ bin "cmp" size rhsString lhsString ""

xor :: EmitM m => Size -> Loc -> Loc -> m ()
xor size src dest =
    let srcString = emitLoc size src
        destString = emitLoc size dest
    in case (src, dest) of
        (LocImm _, LocImm _) -> error "internal error. xor on immediates"
        (LocStack _, LocStack _) -> error "internal error. xor on two stack locs"
        _ -> emitInd $ bin "xor" size srcString destString ""

leaOfConst :: EmitM m => String -> Reg -> m ()
leaOfConst constIdent dest =
    emitInd $ "lea " ++ constIdent ++ "(%rip), " ++ reg (reg64 dest)

neg :: EmitM m => Reg -> m ()
neg reg_ = emitInd $ "neg" ++ sizeSuf Double ++ " " ++ sizedReg Double reg_

setl :: EmitM m => Reg -> m ()
setl reg_ = emitInd $ "setl " ++ sizedReg Byte reg_

setle :: EmitM m => Reg -> m ()
setle reg_ = emitInd $ "setle " ++ sizedReg Byte reg_

setg :: EmitM m => Reg -> m ()
setg reg_ = emitInd $ "setg " ++ sizedReg Byte reg_

setge :: EmitM m => Reg -> m ()
setge reg_ = emitInd $ "setge " ++ sizedReg Byte reg_

sete :: EmitM m => Reg -> m ()
sete reg_ = emitInd $ "sete " ++ sizedReg Byte reg_

setne :: EmitM m => Reg -> m ()
setne reg_ = emitInd $ "setne " ++ sizedReg Byte reg_

test :: EmitM m => Loc -> Loc -> m ()
test op1 op2 =
    let op1String = emitLoc Byte op1
        op2String = emitLoc Byte op2
    in emitInd $ bin "test" Byte op1String op2String ""

jz :: EmitM m => LabIdent -> m ()
jz (LabIdent l) = emitInd $ "jz " ++ sanitise l

idiv :: EmitM m => Size -> Loc -> m ()
idiv size loc = emitInd $ "idiv" ++ sizeSuf size ++ " " ++ emitLoc size loc

cdq :: EmitM m => m ()
cdq = emitInd "cdq"

call :: EmitM m => String -> m ()
call f = emitInd $ "call " ++ sanitise f

push :: EmitM m => Loc -> String -> m ()
push srcloc comment = emitInd $ "push " ++ emitLoc Quadruple srcloc ++ emitComment comment

incrStack :: EmitM m => Int64 -> String -> m ()
incrStack n comment = emitInd $ "subq " ++ lit64 n ++ ", %rsp" ++ emitComment comment

decrStack :: EmitM m => Int64 -> m ()
decrStack n = emitInd $ "addq " ++ lit64 n ++ ", %rsp"

lit :: Int -> String
lit n = '$':show n

lit64 :: Int64 -> String
lit64 n = '$':show n

sizedReg :: Size -> Reg -> String
sizedReg size r = case size of
    Byte      -> reg $ reg8 r
    Double    -> reg $ reg32 r
    Quadruple -> reg $ reg64 r

reg :: String -> String
reg r = '%':r

stack :: Int64 -> String
stack n = show n ++ "(%rbp)"

bin :: String -> Size -> String -> String -> String -> String
bin instr size x y comment = instr ++ sizeSuf size ++ " " ++ x ++ ", " ++ y ++ emitComment comment

sizeSuf :: Size -> String
sizeSuf s = case s of
    Byte      -> "b"
    Double    -> "l"
    Quadruple -> "q"

sanitise :: String -> String
sanitise s = case s of
    []     -> []
    '~':xs -> '_':'_':sanitise xs
    x:xs   -> x:sanitise xs

emitLoc :: Size -> Loc -> String
emitLoc size loc = case loc of
    LocReg r   -> sizedReg size r
    LocStack n -> stack n
    LocImm n   -> lit n
