{-# LANGUAGE FlexibleInstances #-}
module X86_64.CodeGen.Epilogue (withEpilogue) where

import           Data.List           (sortOn)
import           Data.Ord            (Down (Down))
import qualified Data.Set            as Set
import qualified X86_64.CodeGen.Emit as Emit
import           X86_64.CodeGen.GenM
import           X86_64.Loc
import           X86_64.Registers

withEpilogue :: Store -> CompiledMethod -> CompiledMethod
withEpilogue st mthd =
    let savedRegs = sortOn Down $ filter (\r -> regType r == CalleeSaved) $ Set.elems $ usedRegs st
        neededAlignment = odd $ length savedRegs
        epilogue =
            Emit.leave :
            [Emit.decrStack 8 | neededAlignment] ++
            map (Emit.pop . LocReg) savedRegs ++
            [Emit.ret]
    in mthd {mthdEpilogue = map Emit.emitAsString epilogue}
