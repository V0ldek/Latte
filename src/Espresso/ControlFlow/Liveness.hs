module Espresso.ControlFlow.Liveness where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG (CFG (..))
import           Espresso.Syntax.Abs

type VarSet = Set.Set String
data Liveness = Liveness { liveIn :: VarSet, liveOut :: VarSet, liveGen :: VarSet, liveKill :: VarSet }

analyseLiveness :: CFG a -> CFG Liveness
analyseLiveness = undefined

instance Show Liveness where
    show = undefined
