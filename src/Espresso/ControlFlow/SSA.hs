{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module Espresso.ControlFlow.SSA (transformToSSA, unwrapSSA, SSA(..)) where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.ControlFlow.Phi
import           Espresso.Syntax.Abs
import           Identifiers

newtype RenameDictionary = Dict {
    dict :: Map.Map ValIdent ValIdent
} deriving Eq

newtype SSA a = SSA (CFG a) deriving (Eq, Functor)

data RenamedValIdent = NewValIdent {_ident :: ValIdent, version :: Integer}

unwrapSSA :: SSA a -> CFG a
unwrapSSA (SSA g) = g

transformToSSA :: CFG Liveness -> Method () -> SSA ()
transformToSSA g (Mthd _ _ _ params _) =
    let g' = insertEmptyPhis g
        (g'', dicts) = transformLocally g' params
    in SSA $ removeTrivialPhis $ fillPhis g'' dicts

insertEmptyPhis :: CFG Liveness -> CFG ()
insertEmptyPhis = linearMap insertInNode
    where
        insertInNode node = if nodeLabel node == entryLabel then () <$ node else
            let (ls, code) = partition isLabel (nodeCode node)
                ls' = map (() <$) ls
                code' = map (() <$) code
                phis' = map (() <$) (phis node)
            in  node {nodeCode = ls' ++ phis' ++ code'}
        phis node = map fst $ foldl' addPhiVars (emptyPhis node) (Set.elems $ nodeIn node)
        addPhiVars xs n = map (\(IPhi _ vi phiVars, t) -> (IPhi () vi (PhiVar () n (VVal () t vi):phiVars), t)) xs
        emptyPhis node = map (\(vi, (_, t)) -> (IPhi () (ValIdent vi) [], t)) (Map.toList $ liveIn $ nodeLiveness node)

fillPhis :: CFG () -> Map.Map LabIdent RenameDictionary -> CFG ()
fillPhis g dicts = linearMap fillInNode g
    where
        fillInNode node =
            let (ls, x) = partition isLabel (nodeCode node)
                (phis, code) = partition isPhi x
                phis' = map fillPhi phis
            in  node {nodeCode = ls ++ phis' ++ code}
        fillPhi (IPhi _ vi phiVars) =
            IPhi () vi (map fillPhiVar phiVars)
        fillPhi _ = error "impossible"
        fillPhiVar (PhiVar _ n (VVal _ t vi)) =
            let vi' = fromMaybe vi (Map.lookup vi (dict $ dicts Map.! n))
            in  PhiVar () n (VVal () t vi')
        fillPhiVar _ = error "impossible"

type TransformState = (Map.Map ValIdent ValIdent, Map.Map ValIdent RenamedValIdent)
transformLocally :: CFG a -> [Param ()] -> (CFG a, Map.Map LabIdent RenameDictionary)
transformLocally g params =
    let paramMap = Map.fromList (map (\(Param _ _ vi) -> (vi, NewValIdent vi 1)) params)
        g' = Map.fromList $ evalState (mapM transformNode (lineariseNodes g)) (Map.empty, paramMap)
    in (CFG $ Map.map fst g', Map.map snd g')
    where
        transformNode :: Node a -> State TransformState (LabIdent, (Node a, RenameDictionary))
        transformNode node = do
            instrs <- mapM transformInstr (nodeCode node)
            (d, _) <- get
            modify (first (const Map.empty))
            return (nodeLabel node, (node {nodeCode = instrs}, Dict d))
        transformInstr instr = case instr of
            IRet a val -> do
                x <- rename val
                return $ IRet a x
            IOp a vi val1 op val2 -> do
                x1 <- rename val1
                x2 <- rename val2
                vi' <- newVersion vi
                return $ IOp a vi' x1 op x2
            ISet a vi val -> do
                x <- rename val
                vi' <- newVersion vi
                return $ ISet a vi' x
            IStr a vi str -> do
                vi' <- newVersion vi
                return $ IStr a vi' str
            IUnOp a vi op val -> do
                x <- rename val
                vi' <- newVersion vi
                return $ IUnOp a vi' op x
            IVCall a call -> do
                call' <- transformCall call
                return $ IVCall a call'
            ICall a vi call -> do
                call' <- transformCall call
                vi' <- newVersion vi
                return $ ICall a vi' call'
            INew a vi t -> do
                vi' <- newVersion vi
                return $ INew a vi' t
            INewArr a vi t val -> do
                x <- rename val
                vi' <- newVersion vi
                return $ INewArr a vi' t x
            ICondJmp a val l1 l2 -> do
                x <- rename val
                return $ ICondJmp a x l1 l2
            ILoad a vi val -> do
                x <- rename val
                vi' <- newVersion vi
                return $ ILoad a vi' x
            IStore a val1 val2 -> do
                x1 <- rename val1
                x2 <- rename val2
                return $ IStore a x1 x2
            IFld a vi val qi -> do
                x <- rename val
                vi' <- newVersion vi
                return $ IFld a vi' x qi
            IArr a vi val1 val2 -> do
                x1 <- rename val1
                x2 <- rename val2
                vi' <- newVersion vi
                return $ IArr a vi' x1 x2
            IArrLen a vi val -> do
                x <- rename val
                vi' <- newVersion vi
                return $ IArrLen a vi' x
            IPhi a vi phiVars -> do
                vi' <- newVersion vi
                return $ IPhi a vi' phiVars
            _ -> return instr
        transformCall call = case call of
            Call a t qi vals -> do
                xs <- mapM rename vals
                return $ Call a t qi xs
            CallVirt a t qi vals -> do
                xs <- mapM rename vals
                return $ CallVirt a t qi xs
        rename val = case val of
            VVal a t vi -> do
                mbvi' <- gets (Map.lookup vi . fst)
                let vi' = fromMaybe vi mbvi'
                return $ VVal a t vi'
            _ -> return val
        newVersion vi = do
            mbvi' <- gets (Map.lookup vi . snd)
            let version' = case mbvi' of
                    Just vi' -> version vi' + 1
                    Nothing  -> 1
                ident' = ValIdent $ toStr vi ++ if version' == 1 then "" else "~" ++ show version'
            modify (first $ Map.insert vi ident')
            modify (second $ Map.insert vi (NewValIdent ident' version'))
            return ident'
