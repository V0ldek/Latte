{-# LANGUAGE DeriveFoldable   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module Espresso.ControlFlow.CFG (cfg, CFG(..), Node(..), linearize, nodeHead, nodeTail) where

import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities

newtype CFG a = CFG (Map.Map LabIdent (Node a)) deriving (Eq, Functor, Foldable)

data Node a = Node {
    nodeLabel :: LabIdent,
    nodeCode  :: [Instr a],
    nodeOut   :: Set.Set LabIdent,
    nodeIn    :: Set.Set LabIdent
} deriving (Eq, Functor, Foldable)

cfg :: Method a -> CFG a
cfg (Mthd _ _ _ _ instrs) =
    let basicBlocks = splitBasicBlocks instrs
        initial = Map.fromList $ map (\(l, is) -> (l, Node l is Set.empty Set.empty)) basicBlocks
    in  execState (construct basicBlocks) (CFG initial)

linearize :: CFG a -> [Instr a]
linearize (CFG g) = evalState (go (g Map.! entryLabel)) Set.empty
    where
        go node = do
            rest <- mapM expand (Set.elems $ nodeOut node)
            return $ nodeCode node ++ concat rest
        expand l = do
            let node = g Map.! l
            wasVisited <- gets (Set.member l)
            if wasVisited then return [] else do
                modify (Set.insert l)
                go node

nodeHead :: Node a -> a
nodeHead node = let firstInstr = head $ nodeCode node
                in single firstInstr

nodeTail :: Node a -> a
nodeTail node = let lastInstr = last $ nodeCode node
                in single lastInstr

splitBasicBlocks :: [Instr a] -> [(LabIdent, [Instr a])]
splitBasicBlocks = go []
    where
        go bbs []                         = map (second reverse) bbs
        go bbs (i@(ILabel _ l):is)        = go ((l, [i]):bbs) is
        go bbs (i@(ILabelAnn _ l _ _):is) = go ((l, [i]):bbs) is
        go ((l, x):bbs) (j@IJmp{}:is)     = go ((l, j:x):bbs) (goToNextLabel is)
        go ((l, x):bbs) (j@ICondJmp{}:is) = go ((l, j:x):bbs) (goToNextLabel is)
        go ((l, x):bbs) (j@IVRet{}:is)    = go ((l, j:x):bbs) (goToNextLabel is)
        go ((l, x):bbs) (j@IRet{}:is)     = go ((l, j:x):bbs) (goToNextLabel is)
        go ((l, x):bbs) (i:is)            = go ((l, i:x):bbs) is
        go [] _                           = error "empty basic block"
        goToNextLabel (i@ILabel {}:is)    = i:is
        goToNextLabel (i@ILabelAnn {}:is) = i:is
        goToNextLabel (_:is)              = goToNextLabel is
        goToNextLabel []                  = []

construct :: [(LabIdent, [Instr a])] -> State (CFG a) ()
construct = mapM_ fromJumps
    where fromJumps (_, [])      = return ()
          fromJumps (from, i:is) = fromInstr from i >> fromJumps (from, is)
          fromInstr from i = case i of
              IJmp _ to            -> addEdge from to
              ICondJmp _ _ to1 to2 -> addEdge from to1 >> addEdge from to2
              _                    -> return ()

addEdge :: LabIdent -> LabIdent -> State (CFG a) ()
addEdge from to = do
    fromNode <- gets (\(CFG g) -> g Map.! from)
    toNode <- gets (\(CFG g) -> g Map.! to)
    let fromNode' = fromNode {nodeOut = Set.insert to (nodeOut fromNode)}
        toNode' = toNode {nodeIn = Set.insert from (nodeOut toNode)}
    modify (\(CFG g) -> CFG $ Map.insert from fromNode' g)
    modify (\(CFG g) -> CFG $ Map.insert to toNode' g)

instance Show (CFG a) where
    show (CFG g) = unlines (nodes:map edges (Map.elems g))
        where nodes = show (map toStr $ Map.keys g)
              edges node = show (toStr $ nodeLabel node) ++ " -> " ++ show (nodeOut node)
