module Espresso.Optimisation.CFGTransform (inlineTrivialBlocks, removeUnreachable) where

import           Data.List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Espresso.ControlFlow.CFG
import           Espresso.Syntax.Abs
import           Utilities                (fixpointBy)

-- Some labels and jumps between them might be trivial and unnecessary,
-- namely when there are node (v, u) such that v -> u and there are no
-- other outgoing edges from v or incoming edges into u, they can be
-- collapsed to a single node.
inlineTrivialBlocks :: CFG () -> CFG ()
inlineTrivialBlocks = fixpointBy (\(CFG g) -> Map.keys g) collapseOnce

removeUnreachable :: CFG () -> Method () -> (CFG (), Method ())
removeUnreachable cfg_ (Mthd _ t qi ps _) =
    let mthd = Mthd () t qi ps (linearize cfg_)
    in  (cfg mthd, mthd)

collapseOnce :: CFG () -> CFG ()
collapseOnce cfg_@(CFG g) =
    let edges = cfgEdges cfg_
        eligibleEdge = find (\(v, u) -> uniqueOutgoing v && uniqueIncoming u) edges
    in  case eligibleEdge of
            Just (v, u) -> let v' = u `inlineInto` v
                           in  CFG (Map.insert (nodeLabel v') v' $ Map.delete (nodeLabel u) g)
            Nothing     -> cfg_

cfgEdges :: CFG () -> [(Node (), Node ())]
cfgEdges (CFG g) = concatMap (\v -> map (\ui -> (v, g Map.! ui)) (Set.elems $ nodeOut v)) (Map.elems g)

uniqueOutgoing :: Node () -> Bool
uniqueOutgoing n = Set.size (nodeOut n) == 1

uniqueIncoming :: Node () -> Bool
uniqueIncoming n = Set.size (nodeIn n) == 1

inlineInto :: Node () -> Node () -> Node ()
inlineInto u v =
    let vInstrs = filter (\i -> i /= IJmp () (nodeLabel u)) (nodeCode v)
        uInstrs = dropWhile isLabel (nodeCode u)
    in v {nodeCode = vInstrs ++ uInstrs, nodeOut = nodeOut u}

isLabel :: Instr a -> Bool
isLabel instr = case instr of
  ILabel {}    -> True
  ILabelAnn {} -> True
  _            -> False
