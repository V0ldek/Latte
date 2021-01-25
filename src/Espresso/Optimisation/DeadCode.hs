module Espresso.Optimisation.DeadCode (removeDeadCode, removeDeadCodeSSA) where

import qualified Data.HashMap.Strict           as Map
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

-- Remove assignments to dead variables and unreachable code.
removeDeadCode :: CFG Liveness -> CFG Liveness
removeDeadCode = linearMap (\n -> n {nodeCode = loopRemove True (nodeCode n)})

removeDeadCodeSSA :: SSA Liveness -> SSA Liveness
removeDeadCodeSSA (SSA g) = SSA $ removeDeadCode g

loopRemove :: Bool -> [Instr Liveness] -> [Instr Liveness]
loopRemove _ [] = []
loopRemove reachable (instr:instrs) =
    let live = liveOut $ single instr
    in case instr of
            ILabel {}         -> instr:cont True
            ILabelAnn {}      -> instr:cont True
            _ | not reachable -> cont False
            IVRet _           -> instr:cont False
            IRet _ _          -> instr:cont False
            IJmp {}           -> instr:cont False
            ICondJmp {}       -> instr:cont False
            IVCall {}         -> instr:cont True
            ICall {}          -> instr:cont True
            IStore {}         -> instr:cont True
            IEndPhi {}        -> instr:cont True
            IOp _ vi _ _ _
                | toStr vi `Map.member` live -> instr:cont True
            ISet _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IUnOp _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            INew _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            INewArr _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            INewStr _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            ILoad _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IPhi _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            ISwap _ _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            _                 -> cont reachable
    where
        cont b = loopRemove b instrs
