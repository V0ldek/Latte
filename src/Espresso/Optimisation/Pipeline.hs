module Espresso.Optimisation.Pipeline where

import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.SSA
import           Espresso.Optimisation.CommonSubexpressions
import           Espresso.Optimisation.DeadCode
import           Espresso.Optimisation.Propagation
import           Espresso.Syntax.Abs
import           Utilities

-- Main optimisation pipeline.
optimise :: SSA () -> Method () -> SSA ()
optimise ssa mthd = fixpoint ((`propagateCopiesAndConsts` mthd) .
                    (() <$) .
                    globalCommonSubexpressionElimination .
                    removeDeadCodeSSA .
                    analyseLivenessSSA) ssa

analyseLivenessSSA :: SSA () -> SSA Liveness
analyseLivenessSSA (SSA g) = SSA $ analyseLiveness g
