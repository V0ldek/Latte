module SemanticAnalysis.Toplevel (programMetadata, Metadata) where

import           Control.Monad.State

import           Data.Either            (isRight)
import           Data.List              (intercalate)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Error                  (errorMsg)
import           SemanticAnalysis.Class
import           Syntax.Abs

newtype Metadata = Meta [Class]

programMetadata :: Program Pos -> Either String Metadata
programMetadata x = case x of
    Program _ ts -> topDefsMetadata ts

topDefsMetadata :: [TopDef Pos] -> Either String Metadata
topDefsMetadata ts = do
    let fnDefs = filter isFnDef ts
        clDefs = filter isClDef ts
    functions <- fnDefsMetadata fnDefs
    topLevelClass <- clCons topLevelClassIdent Nothing [] functions
    classes <- clDefsMetadata clDefs
    return $ Meta (topLevelClass : classes)
    where isClDef x = case x of
            ClDef {}    -> True
            ClExtDef {} -> True
            FnDef {}    -> False
          isFnDef x = case x of
            FnDef {} -> True
            _        -> False

fnDefsMetadata :: [TopDef Pos] -> Either String [Method]
fnDefsMetadata = mapM fnDefMetadata
    where fnDefMetadata def@(FnDef a typ i args _) =
            let res = funCons typ i args def
            in if isRight res then res else fnErr res
            where fnErr (Left s) = Left $ errorMsg (s ++ "\n") a ("In definition of function `" ++ showI i ++ "`.")

type ClTraversalM = StateT (Map.Map Ident (Maybe Class)) (Either String)

clDefsMetadata :: [TopDef Pos] -> Either String [Class]
clDefsMetadata cls = do
    res <- execStateT run Map.empty
    return $ map fromJust (Map.elems res)
        where
        run :: ClTraversalM [Class]
        run = mapM clMetadata clIdents
        clMetadata :: Ident -> ClTraversalM Class
        clMetadata i = do
            mbEntry <- gets $ Map.lookup i
            case mbEntry of
                Nothing        -> case tsByIdent Map.! i of
                    def@(ClDef _ i _) -> do
                        cl <- defToMetadata def
                        modify $ Map.insert i (Just cl)
                        return cl
                    def@(ClExtDef a i ext _) -> do
                        unless (ext `Map.member` tsByIdent) (undefBaseError a i ext)
                        modify $ Map.insert i Nothing
                        baseCl <- clMetadata ext
                        nonExtCl <- defToMetadata def
                        cl <- case nonExtCl `clExtend` baseCl of
                                Left s -> lift $ Left $ Error.errorMsg (s ++ "\n") a ("In definition of class `" ++ showI i ++ "`.")
                                Right cl -> return cl
                        modify $ Map.insert i (Just cl)
                        return cl
                Just Nothing   -> inhCycleError (unwrap $ tsByIdent Map.! i) i (i : cycle ++ [i])
                        where
                            cycle = takeWhile (/= i) (chain i)
                            chain i = let ClExtDef _ _ ext _ = tsByIdent Map.! i in ext : chain ext
                Just (Just cl) -> return cl
        clIdents = map tdIdent cls
        tsByIdent = Map.fromList $ zip clIdents cls

inhCycleError :: Pos -> Ident -> [Ident] -> ClTraversalM a
inhCycleError a i cycle = lift $ Left $ Error.errorMsg msg a ctx
    where msg = "Cycle detected in inheritance hierarchy: " ++ cycleString ++ "."
          ctx = "In definition of class `" ++ showI i ++ "`."
          cycleString = intercalate " -> " (map (\i -> "`" ++ showI i ++ "`") cycle)

undefBaseError :: Pos -> Ident -> Ident -> ClTraversalM a
undefBaseError a i ext = lift $ Left $ Error.errorMsg msg a ctx
    where msg = "Undefined base class `" ++ showI ext ++ "`."
          ctx = "In definition of class `" ++ showI i ++ "`."

defToMetadata :: TopDef Pos -> ClTraversalM Class
defToMetadata def = do
    let fldDefs = filter isFldDef clDefs
        mthdDefs = filter isMthdDef clDefs
    let flds = map defToFld fldDefs
    mthds <- mapM defToMthd mthdDefs
    let res = clCons i Nothing flds mthds
    lift $ if isRight res then res else clErr res
    where
        (i, a, clDefs) = case def of
            ClDef a i (ClBlock _ defs)      -> (i, a, defs)
            ClExtDef a i _ (ClBlock _ defs) -> (i, a, defs)
        isFldDef x = case x of
            FldDef {} -> True
            _         -> False
        isMthdDef x = case x of
            MthDef {} -> True
            _         -> False
        defToFld def@(FldDef _ typ fldI) = fldCons typ fldI def
        defToMthd def@(MthDef a typ mthdI args _) = lift $
            let res = mthdCons typ mthdI (Ref () $ Cl () i) args def
            in if isRight res then res else mthdErr res
            where mthdErr (Left s) =
                    Left $ errorMsg (s ++ "\n") a ("In definition of method `" ++ showI i ++ "." ++ showI mthdI ++ "`.")
        clErr (Left s) = Left $ errorMsg (s ++ "\n") a ("In definition of class `" ++ showI i ++ "`.")

tdIdent :: TopDef a -> Ident
tdIdent x = case x of
    FnDef _ _ i _ _  -> i
    ClDef _ i _      -> i
    ClExtDef _ i _ _ -> i

instance Show Metadata where
    show (Meta cls) = ".metadata\n\n" ++ intercalate "\n\n" (map show cls)
