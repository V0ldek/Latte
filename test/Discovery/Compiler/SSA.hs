module Compiler.SSA (findMultipleAssignments) where

import           Control.Monad.State
import qualified Data.Set            as Set
import           Espresso.Syntax.Abs

data SSAState a = St {
    assigned         :: Set.Set ValIdent,
    multipleAssigned :: Set.Set (QIdent a, ValIdent)
}

findMultipleAssignments :: Ord a => Program a -> [(QIdent a, ValIdent)]
findMultipleAssignments prog = Set.elems $ multipleAssigned $ execState (go prog) (St Set.empty Set.empty)

go :: Ord a => Program a -> State (SSAState a) ()
go (Program _ _ mthds) = forM_ mthds (\mthd -> do
    goMthd mthd
    modify (\st -> st {assigned = Set.empty}))

goMthd :: Ord a => Method a -> State (SSAState a) ()
goMthd (Mthd _ _ qi ps is) = do
    forM_ (map (\(Param _ _ vi) -> vi) ps) (assign qi)
    forM_ is goInstr
    where
        goInstr instr = case instr of
            IOp _ vi _ _ _   -> assign qi vi
            ISet _ vi _      -> assign qi vi
            IUnOp _ vi _ _   -> assign qi vi
            ICall _ vi _     -> assign qi vi
            INew _ vi _      -> assign qi vi
            INewArr _ vi _ _ -> assign qi vi
            INewStr _ vi _   -> assign qi vi
            ILoad _ vi _     -> assign qi vi
            IPhi _ vi _      -> assign qi vi
            _                -> return ()

assign :: Ord a => QIdent a -> ValIdent -> State (SSAState a) ()
assign qi vi = do
    isAss <- gets (Set.member vi . assigned)
    if isAss
      then modify (\st -> st {multipleAssigned = Set.insert (qi, vi) (multipleAssigned st)})
      else modify (\st -> st {assigned = Set.insert vi (assigned st)})

