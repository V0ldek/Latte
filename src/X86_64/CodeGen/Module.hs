module X86_64.CodeGen.Module where

import           Data.List
import qualified Data.Map              as Map
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Class
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit   as Emit
import           X86_64.CodeGen.GenM
import           X86_64.Size

-- Combine compiled methods to produce a full assembly file as string.
generateModule :: [CompiledClass] -> [CompiledMethod] -> ConstSet -> String
generateModule cls mthds allConsts =
       let code = concatMap (\m -> emitMthd m ++ "\n") mthds
           header = map Emit.extern runtimeSymbols ++
                    [Emit.globalMain | mainEntry `elem` map mthdEntry mthds] ++
                    map Emit.constDef (constsElems allConsts)
           classComments = Emit.commentMultiline $ "Class metadata:":concatMap commentClass cls
       in unlines (map Emit.emitAsString $ header ++ [classComments]) ++ "\n\n" ++ code
    where mainEntry = toStr $
            labelFor (QIdent () (SymIdent $ toStr topLevelClassIdent) (SymIdent $ toStr mainSymIdent)) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code
          commentClass cl = ("Class " ++ toStr (clName cl) ++ ":"):
                             "  Fields:":concatMap commentField (sortOn fldOffset $ Map.elems $ clFlds cl)
          commentField fld = ["    Field name:   " ++ toStr (fldName fld),
                              "    Field type:   " ++ show (fldType fld),
                              "    Field offset: " ++ show (fldOffset fld),
                              "    Field size:   " ++ show (sizeInBytes $ typeSize $ fldType fld)]
