module Espresso.ControlFlow.Phi (unfoldPhi) where

import           Data.List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG
import           Espresso.Syntax.Abs
import           Identifiers

unfoldPhi :: (CFG (), Method ()) -> CFG ()
unfoldPhi (CFG g, Mthd () t qi ps _) =
    let (unfolded, ls) = unzip $ Map.elems $ Map.map (\node -> let l = nodeLabel node in go l (nodeCode node)) g
        rewritten = rerouteJumps (concat ls) (concat unfolded)
    in cfg $ Mthd () t qi ps rewritten
    where
        go l code = let nontrivial = map unfoldTrivialPhi code
                    in createJumpDests l nontrivial

unfoldTrivialPhi :: Instr () -> Instr ()
unfoldTrivialPhi instr = case instr of
    IPhi a i [PhiVar _ _ val] -> ISet a i val
    _                         -> instr

createJumpDests :: LabIdent -> [Instr ()] -> ([Instr ()], [(LabIdent, LabIdent)])
createJumpDests _ [] = error "internal error. empty node"
createJumpDests l (labelInstr:instrs) =
    let (phiInstrs, rest) = span isPhi instrs
        (rewrittenPhis, ls) = unfoldToJumpDests (map (\(IPhi _ vi pvs) -> (vi, pvs)) phiInstrs)
    in if any isPhi rest
        then error "internal error. phi not immediately succeeding a label"
        else if not $ isLabel labelInstr
        then error "internal error. label not first instruction in node"
        else (rewrittenPhis ++ [labelInstr] ++ rest, ls)
    where
        unfoldToJumpDests values =
            let sourceToValuePairs = Map.toList $ foldr accVars Map.empty values
                (code, ls) = unzip $ map createSingleDest sourceToValuePairs
            in (concat code, ls)
        accVars (vi, phiVars) acc =
            foldr (\(PhiVar _ src v) -> Map.insertWith (++) src [(vi, v)]) acc phiVars
        createSingleDest (lSrc, setVals) =
            (ILabel () (phiUnfoldJumpFromToLabel lSrc l) :
            map (uncurry (ISet ())) setVals
            ++ [IJmp () l], (lSrc, l))

isPhi :: Instr a -> Bool
isPhi instr = case instr of
    IPhi {} -> True
    _       -> False

isLabel :: Instr a -> Bool
isLabel instr = case instr of
    ILabel {}    -> True
    ILabelAnn {} -> True
    _            -> False

rerouteJumps :: [(LabIdent, LabIdent)] -> [Instr ()] -> [Instr ()]
rerouteJumps ls instrs = reverse $ fst $ foldl' go ([], entryLabel) instrs
    where
        jumpSet = Set.fromList ls
        go (is, l) instr = case instr of
            ILabel _ l' ->
                (instr:is, l')
            ILabelAnn _ l' _ _ ->
                (instr:is, l')
            IJmp _ lDest ->
                 (IJmp () (reroute l lDest):is, l)
            ICondJmp _ v lDest1 lDest2 ->
                (ICondJmp () v (reroute l lDest1) (reroute l lDest2):is, l)
            _ -> (instr:is, l)
        reroute lSrc lDest = if (lSrc, lDest) `Set.member` jumpSet
                               then phiUnfoldJumpFromToLabel lSrc lDest
                               else lDest
