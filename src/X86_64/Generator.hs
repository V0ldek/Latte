module X86_64.Generator where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG (CFG (..))
import           Espresso.Syntax.Abs

data CompiledMethod = CmpMthd { mthdEntryPoint :: String, mthdCode :: String }

data Const = Const {constIdent :: String, constValue :: String}

data Store = St {
    allCode   :: [String],
    bbCode    :: [String],
    strConsts :: [Const]
}
data Env = Env {
    label :: LabIdent -> String

}

type GenM = StateT Store (Reader Env)

generate :: Method a -> CFG a -> CompiledMethod
generate (Mthd _ qi _) (CFG cfg) = undefined
