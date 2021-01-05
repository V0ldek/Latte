module X86_64.Consts where

import qualified Data.Map as Map

data Const = Const {constIdent :: String, constValue :: String}
newtype ConstSet = ConstSet (Map.Map String Const)

constsEmpty :: ConstSet
constsEmpty = ConstSet Map.empty

constsAdd :: String -> ConstSet -> (Const, ConstSet)
constsAdd s (ConstSet set) =
    let n = Map.size set + 1
        c = Const ("__const_" ++ show n) s
    in case Map.lookup s set of
           Just c' -> (c', ConstSet set)
           Nothing -> (c, ConstSet $ Map.insert s c set)

constsElems :: ConstSet -> [Const]
constsElems (ConstSet set) = Map.elems set
