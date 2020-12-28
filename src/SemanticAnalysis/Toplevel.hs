-- Analyser of toplevel definitions generating class and function metadata.
module SemanticAnalysis.TopLevel (programMetadata, Metadata(..)) where

import           Control.Monad.State
import           Data.List              (intercalate)
import qualified Data.Map               as Map
import           Error                  (errorMsg, errorMsgMb)
import           Identifiers
import           Prelude                hiding (cycle)
import           SemanticAnalysis.Class
import           Syntax.Abs
import           Syntax.Code

newtype Metadata a = Meta (Map.Map Ident (Class a))

-- Analyse the top level definitions in a program and produce its metadata.
programMetadata :: Program Code -> Either String (Metadata Code)
programMetadata x = case x of
    Program _ ts -> topDefsMetadata ts

topDefsMetadata :: [TopDef Code] -> Either String (Metadata Code)
topDefsMetadata ts = do
    let fnDefs = filter isFnDef ts
        clDefs = filter isClDef ts
    functions <- fnDefsMetadata fnDefs
    topLevelClass <- clCons topLevelClassIdent Nothing [] functions
    classes <- clDefsMetadata clDefs
    let allClasses = topLevelClass : rootCl : classes
        clIdents = map clName allClasses
        classMap = Map.fromList (zip clIdents allClasses)
    return $ Meta classMap
    where isClDef x = case x of
            ClDef {}    -> True
            ClExtDef {} -> True
            FnDef {}    -> False
          isFnDef x = case x of
            FnDef {} -> True
            _        -> False

fnDefsMetadata :: [TopDef Code] -> Either String [Method Code]
fnDefsMetadata = mapM fnDefMetadata
    where fnDefMetadata def@(FnDef a typ i args _) =
            let res = funCons typ i args def
            in  case res of
                    Right {} -> res
                    Left s   -> Left $ errorMsg (s ++ "\n") a ("In definition of function `" ++ showI i ++ "`.")
          fnDefMetadata _ = error "invalid def"

-- Monad for traversal of the class inheritance hierarchy.
-- The state keeps all already visited classes and marks which are already
-- resolved and which are currently being resolved. If we ever have a base class
-- that is currently being resolved, we have found a cycle in the hierarchy.
type ClTraversalM = StateT (Map.Map Ident ClassSlot) (Either String)

data ClassSlot = Resolved (Class Code) | Resolving

clDefsMetadata :: [TopDef Code] -> Either String [Class Code]
clDefsMetadata cls = do
    res <- execStateT run Map.empty
    -- After we run we can assume that each class is Resolved (or we failed).
    return $ map (\(Resolved cl) -> cl) (Map.elems res)
        where
        run :: ClTraversalM [Class Code]
        run = mapM clMetadata clIdents
        clMetadata :: Ident -> ClTraversalM (Class Code)
        clMetadata i = do
            mbEntry <- gets $ Map.lookup i
            case mbEntry of
                -- We never visited this class before, start resolution.
                Nothing        -> case tsByIdent Map.! i of
                    def@(ClDef _ i' _) -> do
                        cl <- defToMetadata def
                        modify $ Map.insert i' (Resolved cl)
                        return cl
                    def@(ClExtDef a i' ext _) -> do
                        unless (ext `Map.member` tsByIdent) (undefBaseError (codePos a) i' ext)
                        modify $ Map.insert i' Resolving
                        baseCl <- clMetadata ext
                        nonExtCl <- defToMetadata def
                        cl <- case nonExtCl `clExtend` baseCl of
                                Left s -> lift $ Left $ Error.errorMsg (s ++ "\n") a ("In definition of class `" ++ showI i' ++ "`.")
                                Right cl -> return cl
                        modify $ Map.insert i' (Resolved cl)
                        return cl
                    _ -> error "invalid def"
                -- We already visited this class and were resolving its inheritance chain.
                -- We must have come from a subclass, which implies a cycle in the hierarchy.
                Just Resolving   -> inhCycleError (codePos $ unwrap $ tsByIdent Map.! i) i (i : cycle ++ [i])
                        where
                            cycle = takeWhile (/= i) (chain i)
                            chain i' = let ClExtDef _ _ ext _ = tsByIdent Map.! i' in ext : chain ext
                -- We already resolved this class before, either because it was earlier on the definition list
                -- or one of its subclasses was resolved earlier.
                Just (Resolved cl) -> return cl
        clIdents = map tdIdent cls
        tsByIdent = Map.fromList $ zip clIdents cls

defToMetadata :: TopDef Code -> ClTraversalM (Class Code)
defToMetadata def = do
    let fldDefs = filter isFldDef clDefs
        mthdDefs = filter isMthdDef clDefs
    let flds = map defToFld fldDefs
    mthds <- mapM defToMthd mthdDefs
    let res = clCons i Nothing flds mthds
    lift $ case res of
        Right{} -> res
        Left s ->  Left $ errorMsg (s ++ "\n") a ("In definition of class `" ++ showI i ++ "`.")
    where
        (i, a, clDefs) = case def of
            ClDef a' i' (ClBlock _ defs)      -> (i', a', defs)
            ClExtDef a' i' _ (ClBlock _ defs) -> (i', a', defs)
            _                                 -> error "invalid def"
        isFldDef x = case x of
            FldDef {} -> True
            _         -> False
        isMthdDef x = case x of
            MthDef {} -> True
            _         -> False
        defToFld def'@(FldDef _ typ fldI) = fldCons typ fldI def'
        defToFld _                        = error "invalid def"
        defToMthd def'@(MthDef a' typ mthdI args _) = lift $
            let res = mthdCons typ mthdI (Ref () $ Cl () i) args def'
            in case res of
                Right{} -> res
                Left s -> Left $ errorMsg (s ++ "\n") a' ("In definition of method `" ++ showI i ++ "." ++ showI mthdI ++ "`.")
        defToMthd _ = error "invalid def"

tdIdent :: TopDef a -> Ident
tdIdent x = case x of
    FnDef _ _ i _ _  -> i
    ClDef _ i _      -> i
    ClExtDef _ i _ _ -> i

instance Show (Metadata a) where
    show (Meta cls) = ".metadata\n\n" ++ intercalate "\n\n" (map show $ Map.elems cls)

-- Errors

inhCycleError :: Maybe Pos -> Ident -> [Ident] -> ClTraversalM a
inhCycleError a i cycle = lift $ Left $ Error.errorMsgMb msg a (Just ctx)
    where msg = "Cycle detected in inheritance hierarchy: " ++ cycleString ++ "."
          ctx = "In definition of class `" ++ showI i ++ "`."
          cycleString = intercalate " -> " (map (\i' -> "`" ++ showI i' ++ "`") cycle)

undefBaseError :: Maybe Pos -> Ident -> Ident -> ClTraversalM a
undefBaseError a i ext = lift $ Left $ Error.errorMsgMb msg a (Just ctx)
    where msg = "Undefined base class `" ++ showI ext ++ "`."
          ctx = "In definition of class `" ++ showI i ++ "`."
