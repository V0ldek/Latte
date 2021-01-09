module X86_64.CodeGen.Module where

import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.CodeGen.Consts
import qualified X86_64.CodeGen.Emit   as Emit
import           X86_64.CodeGen.GenM

generateModule :: [CompiledMethod] -> ConstSet -> String
generateModule mthds allConsts =
       let code = concatMap (\m -> emitMthd m ++ "\n") mthds
           header = map Emit.extern runtimeSymbols ++
                    [Emit.globalMain | mainEntry `elem` map mthdEntry mthds] ++
                    map Emit.constDef (constsElems allConsts)
       in unlines (map Emit.emitAsString header) ++ "\n\n" ++ code
    where mainEntry = toStr $
            labelFor (QIdent () (SymIdent $ toStr topLevelClassIdent) (SymIdent $ toStr mainSymIdent)) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code
