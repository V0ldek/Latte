{-# LANGUAGE QuasiQuotes #-}
module X86_64.CodeGen.Prologue (withPrologue) where

import           Data.List
import qualified Data.Map                  as Map
import           Espresso.Syntax.Abs
import           Identifiers               (labelFor)
import           Text.RE.Replace
import           Text.RE.TDFA.String
import qualified X86_64.CodeGen.Emit       as Emit
import           X86_64.CodeGen.GenM
import           X86_64.Loc
import           X86_64.RegisterAllocation
import           X86_64.Registers
import           X86_64.Size

withPrologue :: QIdent a -> RegisterAllocation -> CompiledMethod -> CompiledMethod
withPrologue qi rs mthd =
    let locs = fromIntegral $ numLocals rs * 8
        savedRegs = sort $ filter (\r -> regType r == CalleeSaved) $ usedRegs rs
        needsAlignment = odd $ length savedRegs
        prologue =
            [colouredInterferenceComment, Emit.label (labelFor qi (LabIdent "")) ""] ++
            map (\r -> Emit.push (LocReg r) "") savedRegs ++
            [Emit.incrStack 8 "16 bytes alignment" | needsAlignment] ++
            [
                Emit.push (LocReg rbp) "",
                Emit.mov Quadruple (LocReg rsp) (LocReg rbp) "",
                Emit.incrStack locs "space for locals"
            ]
        -- Access to parameters passed on stack has to be offset by 8 for each saved
        -- register, including rbp. Additionally correct for alignment.
        paramOffset = 8 * (length savedRegs + 1 + if needsAlignment then 1 else 0)
        newCode = offsetStackParamReferences paramOffset (mthdCode mthd)
        colouredInterferenceComment = Emit.commentMultiline ("Register allocation:":lines (show $ Map.toList $ regAlloc rs))
    in mthd {mthdPrologue = map Emit.emitAsString prologue, mthdCode = newCode}

offsetStackParamReferences :: Int -> [String] -> [String]
offsetStackParamReferences offset = map go
    where go line =
            let pattern_ = [re|([[:space:]])([0-9]+)\(%rbp\)|]
                ms = line *=~ pattern_
                repl _ loc capt =
                    if getCaptureOrdinal (locationCapture loc) == 2
                        then let base = read $ capturedText capt
                             in  Just $ show (base + offset)
                        else Nothing
            in  replaceAllCaptures SUB repl ms
