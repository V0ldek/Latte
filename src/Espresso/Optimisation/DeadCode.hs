module Espresso.Optimisation.DeadCode where

import qualified Data.Map                      as Map
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

removeDeadCode :: SSA Liveness -> SSA Liveness
removeDeadCode (SSA g) = SSA $ linearMap (\n -> n {nodeCode = loopRemove True (nodeCode n)}) g

loopRemove :: Bool -> [Instr Liveness] -> [Instr Liveness]
loopRemove _ [] = []
loopRemove reachable (instr:instrs) =
    let live = liveOut $ single instr
    in case instr of
            ILabel {}         -> instr:cont True
            ILabelAnn {}      -> instr:cont True
            _ | not reachable -> cont reachable
            IVRet _           -> instr:cont False
            IRet _ _          -> instr:cont False
            IJmp {}           -> instr:cont False
            ICondJmp {}       -> instr:cont False
            IVCall {}         -> instr:cont True
            ICall {}          -> instr:cont True
            IStore {}         -> instr:cont True
            IOp _ vi _ _ _
                | toStr vi `Map.member` live -> instr:cont True
            ISet _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IStr _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IUnOp _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            INew _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            INewArr _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            ILoad _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IFld _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            IArr _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            IArrLen _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IPhi _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            _                 -> cont reachable
    where
        cont b = loopRemove b instrs
