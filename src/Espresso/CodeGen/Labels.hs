module Espresso.CodeGen.Labels (
    andLabels,
    condLabels,
    condElseLabels,
    mbAnnLabel,
    orLabels,
    whileLabels
) where

import           Espresso.CodeGen.GenM (GenM, freshLabelIdx)
import           Espresso.Syntax.Abs
import           Identifiers           (labIdent)

-- Create a LabIdent from a root string and an index.
idxLabel :: String -> Integer -> LabIdent
idxLabel s n = labIdent (s ++ show n)

-- Create an annotated label if the line interval is Just.
-- Otherwise, a regular label without annotations.
mbAnnLabel :: LabIdent -> Maybe (Int, Int) -> Instr ()
mbAnnLabel l Nothing         = ILabel () l
mbAnnLabel l (Just (lf, lt)) = ILabelAnn () l (toInteger lf) (toInteger lt)

andLabels :: GenM (LabIdent, LabIdent)
andLabels = twoLabels "true" "false"

condLabels :: GenM (LabIdent, LabIdent)
condLabels = twoLabels "then" "after"

condElseLabels :: GenM (LabIdent, LabIdent, LabIdent)
condElseLabels = threeLabels "then" "else" "after"

orLabels :: GenM (LabIdent, LabIdent)
orLabels = twoLabels "true" "false"

whileLabels :: GenM (LabIdent, LabIdent, LabIdent)
whileLabels = threeLabels "cond" "body" "after"

twoLabels :: String -> String -> GenM (LabIdent, LabIdent)
twoLabels l1 l2 = do
    lidx <- freshLabelIdx
    return (idxLabel l1 lidx, idxLabel l2 lidx)

threeLabels :: String -> String -> String -> GenM (LabIdent, LabIdent, LabIdent)
threeLabels l1 l2 l3 = do
    lidx <- freshLabelIdx
    return (idxLabel l1 lidx, idxLabel l2 lidx, idxLabel l3 lidx)
