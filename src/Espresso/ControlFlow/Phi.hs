module Espresso.ControlFlow.Phi (unfoldPhi) where

import qualified Data.Map                 as Map
import           Espresso.ControlFlow.CFG
import           Espresso.Syntax.Abs

unfoldPhi :: CFG a -> CFG a
unfoldPhi (CFG g) = CFG $ Map.map (\node -> node {nodeCode = map fixTrivialPhi (nodeCode node)}) g

fixTrivialPhi :: Instr a -> Instr a
fixTrivialPhi instr = case instr of
    IPhi a i [PhiVar _ _ val] -> ISet a i val
    _                         -> instr
