-- Unfolding of trivial phi functions done at the end of SSA transformation.
-- Final elimination of phi functions depends on register allocation, and thus
-- is located in X86_64.Phi.
module Espresso.ControlFlow.Phi (removeTrivialPhis, unfoldTrivialPhi) where

import           Data.Maybe
import           Espresso.ControlFlow.CFG
import           Espresso.Syntax.Abs

removeTrivialPhis :: CFG () -> CFG ()
removeTrivialPhis = linearMap (\n -> n {nodeCode = mapMaybe unfoldTrivialPhi (nodeCode n)})

unfoldTrivialPhi :: Instr () -> Maybe (Instr ())
unfoldTrivialPhi instr = case instr of
    IPhi _ _ []               -> Nothing
    -- All variants are the same as destination (empty assignments).
    IPhi _ i phiVars | all (\(PhiVar _ _ val) -> isVal val i) phiVars -> Nothing
    -- All variants are the same (simple set).
    IPhi _ i (PhiVar _ _ val:phiVars)
        | all (\(PhiVar _ _ val') -> val == val') phiVars -> Just $ ISet () i val
    -- Only one variant (simple set).
    IPhi _ i [PhiVar _ _ val] -> Just $ ISet () i val
    -- Remove all empty assignment variants.
    IPhi _ i vars -> Just $ IPhi () i (filter (\(PhiVar _ _ val) -> not $ isVal val i) vars)
    _                         -> Just instr

isVal :: Val () -> ValIdent -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False
