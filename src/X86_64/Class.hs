module X86_64.Class where

import           Data.Int            (Int64)
import           Data.List           (foldl', sortOn)
import qualified Data.Map            as Map
import           Data.Ord            (Down (Down))
import           Espresso.Syntax.Abs
import           X86_64.Size         (sizeInBytes, typeSize)

data CompiledClass = CompiledCl {
    clName :: SymIdent,
    clFlds :: Map.Map SymIdent CompiledField,
    clSize :: Int64
}

data CompiledField = Fld {
    fldName   :: SymIdent,
    fldType   :: SType (),
    fldOffset :: Int64
}

compileClass :: ClassDef a -> CompiledClass
compileClass (ClDef _ i fldDefs _) =
    let (flds, unalignedSize) = layoutFields fldDefs
    in  CompiledCl i (Map.fromList $ map (\f -> (fldName f, f)) flds) (alignSize unalignedSize)

alignSize :: Int64 -> Int64
alignSize n = if n `mod` 8 == 0
                then n
                else n + (8 - n `mod` 8)

layoutFields :: [FieldDef a] -> ([CompiledField], Int64)
layoutFields fldDefs =
    let fldBase = map (\(FldDef _ t sym) -> Fld sym (() <$ t) 0) fldDefs
        ordered = sortOn (Down . typeSize . fldType) fldBase
    in  foldl' go ([], 0) ordered
    where
        go (flds, offset) fld = (fld{fldOffset = offset}:flds, offset + sizeInBytes (typeSize (fldType fld)))
