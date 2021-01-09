-- Sets of string constants.
module X86_64.CodeGen.Consts where

import qualified Data.Map    as Map
import           Identifiers

-- Represents a string constant c of the form:
-- <constName c>:
--     .string "<constValue c>"
data Const = Const {constName :: String, constValue :: String}

-- Set of string constants where each literal string is guaranteed
-- to be uniquely mapped to a named constant.
newtype ConstSet = ConstSet (Map.Map String Const)

-- Empty set.
constsEmpty :: ConstSet
constsEmpty = ConstSet Map.empty

-- Add the given literal to the set. If it is already
-- part of the set the associated constant is returned
-- and the set is unchanged.
constsAdd :: String -> ConstSet -> (Const, ConstSet)
constsAdd s (ConstSet set) =
    let n = Map.size set + 1
        c = Const (constIdent $ show n) s
    in case Map.lookup s set of
           Just c' -> (c', ConstSet set)
           Nothing -> (c, ConstSet $ Map.insert s c set)

-- All constants in the set.
constsElems :: ConstSet -> [Const]
constsElems (ConstSet set) = Map.elems set
