module Espresso.Utilities where

import           Espresso.Syntax.Abs
import           Identifiers               (topLevelClassIdent)
import           SemanticAnalysis.Analyser (SemData (semCode))
import           SemanticAnalysis.Class    as Class (Method (mthdName, mthdSelf),
                                                     showType)
import qualified Syntax.Abs                as Latte
import           Syntax.Code               (Code (codePos))

toSymIdent :: Latte.Ident -> SymIdent
toSymIdent (Latte.Ident s) = SymIdent s

mthdQIdent :: Class.Method a -> QIdent ()
mthdQIdent mthd =
    let mName = (toSymIdent $ mthdName mthd)
    in case mthdSelf mthd of
        Just t  -> QIdent () (SymIdent $ showType t) mName
        Nothing -> QIdent () (toSymIdent topLevelClassIdent) mName

-- Get the span in code lines of a semantically analysed piece of syntax.
codeLines :: (Functor f, Foldable f) => f SemData -> Maybe (Int, Int)
codeLines stmt = do
    lMin <- foldr (lift_ min . semToLine) Nothing stmt
    lMax <- foldr (lift_ max . semToLine) Nothing stmt
    return (lMin, lMax)
    where semToLine s = fst <$> (semCode s >>= codePos)
          lift_ _ Nothing x         = x
          lift_ _ x Nothing         = x
          lift_ f (Just x) (Just y) = Just $ f x y
