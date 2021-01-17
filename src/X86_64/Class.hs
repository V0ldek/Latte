module X86_64.Class where

import           Data.Int            (Int64)
import           Data.List           (foldl')
import qualified Data.Map            as Map
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Size         (sizeInBytes, typeSize)

data CompiledClass = CompiledCl {
    clName   :: SymIdent,
    clFlds   :: Map.Map SymIdent CompiledField,
    clSize   :: Int64,
    clVTable :: VTable
}

data CompiledField = Fld {
    fldName   :: SymIdent,
    fldType   :: SType (),
    fldOffset :: Int64
}

newtype VTable = VTab {
    vtabMthds :: Map.Map SymIdent (String, Int64)
}

compileClass :: ClassDef a -> CompiledClass
compileClass (ClDef _ i fldDefs mthdDefs) =
    let (flds, unalignedSize) = layoutFields fldDefs
        vTable = generateVTable mthdDefs
    in  CompiledCl i (Map.fromList $ map (\f -> (fldName f, f)) flds) (alignSize unalignedSize) vTable

alignSize :: Int64 -> Int64
alignSize n = if n `mod` 8 == 0
                then n
                else n + (8 - n `mod` 8)

layoutFields :: [FieldDef a] -> ([CompiledField], Int64)
layoutFields fldDefs =
    let fldBase = map (\(FldDef _ t sym) -> Fld sym (() <$ t) 0) fldDefs
    in  foldl' go ([], 8) fldBase
    where
        go (flds, offset) fld =
            let fldSize = sizeInBytes (typeSize (fldType fld))
                padding = if offset `mod` fldSize == 0
                            then 0
                            else fldSize - (offset `mod` fldSize)
            in (fld{fldOffset = offset + padding}:flds, offset + padding + fldSize)

generateVTable :: [MethodDef a] -> VTable
generateVTable mthdDefs = VTab $ Map.fromList $
    zipWith (\(MthdDef _ _ qi@(QIdent _ _ si)) idx -> (si, (getCallTarget qi, idx * 8))) mthdDefs [0..]
