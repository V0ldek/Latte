module X86_64.RegisterAllocation.InterferenceGraph (
    buildInterferenceGraph,
    InterferenceGraph(..),
    InterferenceNode(..),
    Colour(..)
) where

import           Control.Monad.State
import           Data.Int
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness
import           Espresso.Syntax.Abs
import           Identifiers
import           Utilities
import           X86_64.Loc
import           X86_64.Registers

newtype InterferenceGraph = IG {
    ig :: Map.Map String InterferenceNode
}

data InterferenceNode = IN {
    iNodeLabel  :: String,
    iNodeColour :: Maybe Colour,
    iNodeOut    :: Set.Set String
}

data Colour = ColReg Reg
            | ColStack Integer
            | ColParamStack Int64
    deriving (Show, Eq, Ord)

data InterferenceState = St {
    stIg   :: InterferenceGraph,
    stDivs :: Integer
}

instance Show InterferenceGraph where
    show (IG g) = unlines $ Map.elems $ Map.map showOne g
        where
        showOne node = iNodeLabel node ++ " " ++ showColour (iNodeColour node) ++ ": " ++ show (Set.elems (iNodeOut node))
        showColour Nothing    = ""
        showColour (Just col) = "(precoloured: " ++ show col ++ ")"

buildInterferenceGraph :: CFG Liveness -> Method a -> InterferenceGraph
buildInterferenceGraph g (Mthd _ _ _ ps _) = stIg $ execState (goParams >> forM_ (linearise g) go) (St (IG Map.empty) 0)
    where
        goParams = forM_ (zip ps (map argLoc [0..])) (\(Param _ _ vi, loc) -> precolourI vi (locToCol loc))
        locToCol loc = case loc of
            LocReg reg_ -> ColReg reg_
            LocStack n  -> ColParamStack n
            _ -> error $ "internal error. invalid param loc " ++ show loc
        go instr = do
            let live = single instr
            case instr of
                IRet _ val             -> precolour val (ColReg rax)
                IOp _ vi _ (OpDiv _) rhs | not $ isSimpleDiv rhs -> do
                    (n1, n2) <- createDivNodes
                    let interfering = n1:n2:filter (/= toStr vi) (Map.keys $ liveOut live)
                    addEdgesBetween interfering
                    precolourI vi (ColReg rax)
                IOp _ vi _ (OpMod _) rhs | not $ isSimpleDiv rhs -> do
                    (n1, n2) <- createDivNodes
                    let interfering = n1:n2:filter (/= toStr vi) (Map.keys $ liveOut live)
                    addEdgesBetween interfering
                    precolourI vi (ColReg rdx)
                IPhi {} -> error "internal error. phi should be eliminated before interference graph"
                _ -> return ()
            addEdgesBetween (Map.keys $ liveOut live)

createDivNodes :: State InterferenceState (String, String)
createDivNodes = do
    n <- gets stDivs
    modify (\st -> st{stDivs = n + 1})
    let nRes = "~div_" ++ show n ++ "_rax"
        nRem = "~div_" ++ show n ++ "_rdx"
        nodeRes = IN nRes (Just $ ColReg rax) Set.empty
        nodeRem = IN nRem (Just $ ColReg rdx) Set.empty
    modify (\st -> st{stIg = IG $ Map.insert nRes nodeRes $ ig $ stIg st})
    modify (\st -> st{stIg = IG $ Map.insert nRem nodeRem $ ig $ stIg st})
    addEdge nRes nRem
    addEdge nRem nRes
    return (nRes, nRem)

precolour :: Val a -> Colour -> State InterferenceState ()
precolour val col = case val of
    VVal _ _ vi -> precolourI vi col
    _           -> return ()

precolourI :: ValIdent -> Colour -> State InterferenceState ()
precolourI (ValIdent vi) col = do
    node <- getNode vi
    let node' = node {iNodeColour = Just col}
    modify (\st -> st{stIg = IG $ Map.insert vi node' $ ig $ stIg st})

addEdgesBetween :: [String] -> State InterferenceState ()
addEdgesBetween xs =
    let edges = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]
    in forM_ edges (uncurry addEdge)

addEdge :: String -> String -> State InterferenceState ()
addEdge n1 n2 = do
    node1 <- getNode n1
    let node1' = node1 {iNodeOut = Set.insert n2 (iNodeOut node1)}
    modify (\st -> st{stIg = IG $ Map.insert n1 node1' $ ig $ stIg st})

getNode :: String -> State InterferenceState InterferenceNode
getNode n = do
    mbnode <- gets (Map.lookup n . ig . stIg)
    case mbnode of
        Just node -> return node
        Nothing -> do
            let node = IN n Nothing Set.empty
            modify (\st -> st{stIg = IG $ Map.insert n node $ ig $ stIg st})
            return node

isSimpleDiv :: Val a -> Bool
isSimpleDiv val = case val of
    VInt _ n -> isPowerOfTwo n
    _        -> False
