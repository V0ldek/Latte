-- Unfolding of the phony phi function to make code viable for assembly codegen.
module Espresso.ControlFlow.Phi (removeTrivialPhis, unfoldPhi, unfoldTrivialPhi) where

import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG
import           Espresso.Syntax.Abs
import           Identifiers

data JumpRoute = JmpRt LabIdent LabIdent deriving (Eq, Ord)

-- For a given method and its CFG, turn the code into an equivalent
-- version without any IPhi instructions and return the new CFG.
unfoldPhi :: (CFG (), Method ()) -> CFG ()
unfoldPhi (g, Mthd () t qi ps _) =
    let (unfolded, jmpRoutes) = unzip $ map go $ lineariseNodes g
        rewritten = rerouteJumps (concat jmpRoutes) (concat unfolded)
    in cfg $ Mthd () t qi ps rewritten
    where
        go node = let l = nodeLabel node
                      code = nodeCode node
                      nontrivial = mapMaybe unfoldTrivialPhi code
                  in createJumpDests l nontrivial

removeTrivialPhis :: CFG () -> CFG ()
removeTrivialPhis = linearMap (\n -> n {nodeCode = mapMaybe unfoldTrivialPhi (nodeCode n)})

unfoldTrivialPhi :: Instr () -> Maybe (Instr ())
unfoldTrivialPhi instr = case instr of
    IPhi _ _ []               -> Nothing
    IPhi _ i phiVars | all (\(PhiVar _ _ val) -> isVal val i) phiVars -> Nothing
    IPhi _ i (PhiVar _ _ val@(VVal _ _ vi):phiVars)
        | all (\(PhiVar _ _ val') -> isVal val' vi) phiVars -> Just $ ISet () i val
    IPhi _ i [PhiVar _ _ val] -> Just $ ISet () i val
    _                         -> Just instr

-- Unwrap sequences of phi instructions by creating a special block for each incoming
-- label that sets the values specified in the variants for that label and then jumps
-- to the start of the original block. The phi instructions must immediatelly
-- succeed the starting label of the block, or this function will fail.
-- For example, the code:
{-
    .L_label:
        %v_a := phi(.L_source1: int %v_0, .L_source2: int %v_1);
        %v_b := phi(.L_source1: 42, .L_source2: int %v_2);
        <code>
-}
-- gets rewritten into:
{-
    .L_label__from_source1:
        %v_a := int %v_0;
        %v_b := 42;
        jump .L_label;
    .L_label__from_source2:
        %v_a := int %v_1;
        %v_b := int %v_2;
        jump .L_label;
    .L_label:
        <code>
-}
createJumpDests :: LabIdent -> [Instr ()] -> ([Instr ()], [JumpRoute])
createJumpDests _ [] = error "internal error. empty node"
createJumpDests l (labelInstr:instrs) =
    let (phiInstrs, rest) = span isPhi instrs
        (rewrittenPhis, jmpRoutes) = unfoldToJumpDests (map (\(IPhi _ vi pvs) -> (vi, pvs)) phiInstrs)
    in if any isPhi rest
        then error "internal error. phi not immediately succeeding a label"
        else if not $ isLabel labelInstr
        then error "internal error. label not first instruction in node"
        else (rewrittenPhis ++ [labelInstr] ++ rest, jmpRoutes)
    where
        unfoldToJumpDests values =
            let sourceToValuePairs = Map.toList $ foldr accVars Map.empty values
                (code, ls) = unzip $ map createSingleDest sourceToValuePairs
            in (concat code, ls)
        accVars (vi, phiVars) acc =
            foldr (\(PhiVar _ src v) m -> if isVal v vi then m else Map.insertWith (++) src [(vi, v)] m) acc phiVars
        createSingleDest (lSrc, setVals) =
            (ILabel () (phiUnfoldJumpFromToLabel lSrc l) :
            map (uncurry (ISet ())) setVals
            ++ [IJmp () l], JmpRt lSrc l)

-- For the routes created by unfolding phi reroute each direct jump from
-- a block to an affected block so that it targets the newly created special
-- labels. So, assuming the same example .L_label as in createJumpDests, the following
-- code:
{-
    .L_source1:
        <code>
        jump .L_label;
    .L_source2:
        <code>
        jump if %v_cond then .L_label else .L_some_other_label;
-}
-- gets rewritten into:
{-
    .L_source1:
        <code>
        jump .L_label__from_source1;
    .L_source2:
        <code>
        jump if %v_cond then .L_label__from_source2 else .L_some_other_label;
-}
rerouteJumps :: [JumpRoute] -> [Instr ()] -> [Instr ()]
rerouteJumps jmpRoutes instrs = reverse $ fst $ foldl' go ([], entryLabel) instrs
    where
        jumpSet = Set.fromList jmpRoutes
        go (is, lSrc) instr = case instr of
            ILabel _ lSrc' ->
                (instr:is, lSrc')
            ILabelAnn _ lSrc' _ _ ->
                (instr:is, lSrc')
            IJmp _ lDest ->
                 (IJmp () (reroute lSrc lDest):is, lSrc)
            ICondJmp _ v lDest1 lDest2 ->
                (ICondJmp () v (reroute lSrc lDest1) (reroute lSrc lDest2):is, lSrc)
            _ ->
                (instr:is, lSrc)
        reroute lSrc lDest =
            if JmpRt lSrc lDest `Set.member` jumpSet
              then phiUnfoldJumpFromToLabel lSrc lDest
              else lDest

isVal :: Val () -> ValIdent -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False
