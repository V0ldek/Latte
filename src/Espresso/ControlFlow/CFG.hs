-- Generation of Control Flow Graphs for methods.
{-# LANGUAGE DeriveFoldable   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module Espresso.ControlFlow.CFG (
    CFG(..),
    Node(..),
    cfg,
    linearize,
    nodeHead,
    nodeTail
) where

import           Control.Monad.State
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Espresso.Syntax.Abs
import           Identifiers         (ToString (toStr), entryLabel)
import           Utilities           (single)

-- A CFG is a set of nodes representing basic blocks, identified by their starting labels.
newtype CFG a = CFG (Map.Map LabIdent (Node a)) deriving (Eq, Functor, Foldable)

data Node a = Node {
    -- Starting label of this basic block.
    nodeLabel :: LabIdent,
    -- All code in this basic block.
    nodeCode  :: [Instr a],
    -- All basic blocks reachable from this block.
    nodeOut   :: Set.Set LabIdent,
    -- All basic blocks that can reach this block.
    nodeIn    :: Set.Set LabIdent
} deriving (Eq, Functor, Foldable)

-- Convert an Espresso method to a CFG. Code that is unreachable within a basic block,
-- that is instructions occuring after a jump, are removed.
cfg :: Method a -> CFG a
cfg (Mthd _ _ _ _ instrs) =
    let basicBlocks = splitBasicBlocks instrs
        initial = Map.fromList $ map (\(l, is) -> (l, Node l is Set.empty Set.empty)) basicBlocks
    in  execState (construct basicBlocks) (CFG initial)

-- Convert a CFG to a sequence of instructions. It is guaranteed to start with the
-- .L_entry block. Blocks that are unreachable from the entry block will be ignored.
linearize :: CFG a -> [Instr a]
linearize (CFG g) =
    case Map.lookup entryLabel g of
        Just entry -> evalState (go entry) Set.empty
        Nothing    -> error "internal error. malformed graph, no entry label"
    where
        go node = do
            rest <- mapM expand (Set.elems $ nodeOut node)
            return $ nodeCode node ++ concat rest
        expand l = do
            case Map.lookup l g of
                Just node -> do
                    wasVisited <- gets (Set.member l)
                    if wasVisited then return [] else do
                        modify (Set.insert l)
                        go node
                Nothing -> error $ "internal error. malformed graph, no " ++ toStr l ++ " node"

-- Extract the element from the first instruction in the block.
nodeHead :: Node a -> a
nodeHead node = let firstInstr = head $ nodeCode node
                in single firstInstr

-- Extract the element from the last instruction in the block.
nodeTail :: Node a -> a
nodeTail node = let lastInstr = last $ nodeCode node
                in single lastInstr

-- Split the instruction sequence into basic blocks. A basic block spans from a label
-- to the first consecutive jump instruction. All instructions after a jump are dead
-- code and are ignored.
splitBasicBlocks :: [Instr a] -> [(LabIdent, [Instr a])]
splitBasicBlocks = go []
    where
        go bbs []                         = map finalizeBlock bbs
        go bbs (i@(ILabel _ l):is)        = go ((l, [i]):bbs) is
        go bbs (i@(ILabelAnn _ l _ _):is) = go ((l, [i]):bbs) is
        go ((l, x):bbs) (j:is) | isJump j = go ((l, j:x):bbs) (dropWhile (not . isLabel) is)
        go ((l, x):bbs) (i:is)            = go ((l, i:x):bbs) is
        go [] _                           = error "first instruction is not a label"
        finalizeBlock (l, []) = error ("empty basic block: " ++ toStr l)
        finalizeBlock (l, is@(i:_)) | isJump i = (l, reverse is)
        finalizeBlock (l, _)  = error ("basic block not ending with a jump: " ++ toStr l)

-- Constructs a CFG from basic blocks.
construct :: [(LabIdent, [Instr a])] -> State (CFG a) ()
construct = mapM_ fromJumps
    where fromJumps (_, [])      = return ()
          fromJumps (from, i:is) = fromInstr from i >> fromJumps (from, is)
          fromInstr from i = case i of
              IJmp _ to            -> addEdge from to
              ICondJmp _ _ to1 to2 -> addEdge from to1 >> addEdge from to2
              _                    -> return ()

-- Add an edge between two blocks to the current CFG.
addEdge :: LabIdent -> LabIdent -> State (CFG a) ()
addEdge from to = do
    mbfromNode <- gets (\(CFG g) -> Map.lookup from g)
    mbtoNode <- gets (\(CFG g) -> Map.lookup to g)
    let fromNode' = case mbfromNode of
            Just fromNode -> fromNode {nodeOut = Set.insert to (nodeOut fromNode)}
            Nothing -> error $ "internal error. no src label " ++ toStr from
        toNode' = case mbtoNode of
            Just toNode -> toNode {nodeIn = Set.insert from (nodeOut toNode)}
            Nothing     -> error $ "internal error. no dest label " ++ toStr to
    modify (\(CFG g) -> CFG $ Map.insert from fromNode' g)
    modify (\(CFG g) -> CFG $ Map.insert to toNode' g)

instance Show (CFG a) where
    show (CFG g) = unlines (nodes:map edges (Map.elems g))
        where nodes = show (map toStr $ Map.keys g)
              edges node = show (toStr $ nodeLabel node) ++ " -> " ++ show (nodeOut node)

isLabel :: Instr a -> Bool
isLabel instr = case instr of
  ILabel {}    -> True
  ILabelAnn {} -> True
  _            -> False

isJump :: Instr a -> Bool
isJump instr = case instr of
    IJmp {}     -> True
    ICondJmp {} -> True
    IVRet {}    -> True
    IRet {}     -> True
    _           -> False
