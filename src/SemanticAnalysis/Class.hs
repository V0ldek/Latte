{-# LANGUAGE DeriveFunctor #-}
-- Definitions of internal metadata types for language entities.
module SemanticAnalysis.Class (
    Class (..),
    Field (..),
    Method (..),
    mthdType,
    mthdTypeIgnSelf,
    clCons,
    rootCl,
    rootType,
    fldCons,
    funCons,
    mthdCons,
    clExtend,
    showType
) where

import           Control.Monad (unless)
import           Data.List     (intercalate, sort)
import qualified Data.Map      as Map
import           Data.Maybe
import           Error         (lineInfo)
import           Identifiers
import           Syntax.Abs
import           Syntax.Code
import           Utilities

data Class a = Class {
    clName    :: Ident,
    clBase    :: Maybe (Class a),
    clFields  :: Map.Map Ident Field,
    clMethods :: [Method a]
} deriving Functor

data Field = Fld { fldName :: Ident, fldType :: Type (), fldCode :: ClDef Code}

data Method a = Mthd {
     mthdName :: Ident,
     mthdRet  :: Type (),
     mthdSelf :: Maybe (Type ()),
     mthdArgs :: [Arg Code],
     mthdBlk  :: Block a,
     mthdCode :: Code
} deriving Functor

-- Get the type of the method, including the hidden `self` parameter.
mthdType :: Method a -> Type ()
mthdType mthd = case mthdSelf mthd of
    Nothing  -> mthdTypeIgnSelf mthd
    Just typ -> let Fun _ ret args = mthdTypeIgnSelf mthd
                in  Fun () ret (typ : args)

-- Get the type of the method, but without the hidden `self` parameter.
mthdTypeIgnSelf :: Method a -> Type ()
mthdTypeIgnSelf (Mthd _ ret _ args _ _) = Fun () ret (map (\(Arg _ t _) -> () <$ t) args)

-- Phony class serving as a root of the inheritance hierarchy.
rootCl :: Class a
rootCl = Class (Ident "~object") Nothing Map.empty []

rootType :: Type ()
rootType = Cl () (clName rootCl)

-- Construct a class from its constituents, checking name rule violations.
clCons :: Ident -> Maybe (Class a) -> [Field] -> [Method a]  -> Either String (Class a)
clCons name base flds mthds = do
    let dupFlds = snd $ findDupsBy (showI . fldName) flds
        dupMthds = snd $ findDupsBy (showI . mthdName) mthds
        conflicts = snd $ findConflictsBy (showI . fldName) (showI . mthdName) flds mthds
        reservedFlds = filter (\f -> fldName f `elem` reservedNames) flds
        reservedMthds = filter (\m -> mthdName m `elem` reservedNames) mthds
    unless (null dupFlds) (dupFldsError dupFlds)
    unless (null dupMthds) (dupMthdsError dupMthds)
    unless (null conflicts) (conflFldsAndMthdsError conflicts)
    unless (null reservedFlds) (reservedFldError reservedFlds)
    unless (null reservedMthds) (reservedMthdsError reservedMthds)
    let base' = if isNothing base then Just rootCl else Nothing
        fldMap = Map.fromList $ map (\f -> (fldName f, f)) flds
    return $ Class name base' fldMap mthds

-- Construct a field from ist constituents.
fldCons :: Type a -> Ident -> ClDef Code -> Field
fldCons typ i = Fld i (() <$ typ)

-- Construct a method representing a top level function from its constituents.
-- Checks parameter naming rules.
funCons :: Type a -> Ident -> [Arg Code] -> TopDef Code -> Either String (Method Code)
funCons typ i args def = do
    let FnDef _ _ _ _ blk = def
    cons <- baseMethodCons typ i Nothing args
    return $ cons blk (unwrap def)

-- Construct a method representing a class method from its constituents.
-- Checks parameter naming rules.
mthdCons :: Type a -> Ident -> Type b -> [Arg Code] -> ClDef Code -> Either String (Method Code)
mthdCons typ i selfTyp args def = do
    let MthDef _ _ _ _ blk = def
    cons <- baseMethodCons typ i (Just selfTyp) args
    return $ cons blk (unwrap def)

-- Common constructor for both top level functions and class methods.
baseMethodCons :: Type a -> Ident -> Maybe (Type b) -> [Arg Code] -> Either String (Block Code -> Code -> Method Code)
baseMethodCons typ i selfTyp args = do
    let dupArgs = fst $ findDupsBy (\(Arg _ _ i') -> showI i') args
    unless (null dupArgs) (dupArgsError dupArgs)
    return $ Mthd i (() <$ typ) (fmap (() <$) selfTyp) args

-- Extend a given base class with the given class.
-- In other words, set the base of the given class and fill its
-- field and method tables according to inheritance rules.
clExtend :: Class Code -> Class Code -> Either String (Class Code)
clExtend cl base = do
    flds <- combinedFlds
    mthds <- combinedMthds
    return $ Class (clName cl) (Just base) flds mthds
    where combinedFlds =
            let flds = clFields base `Map.union` clFields cl
                (_, dups) = findDupsBy fldName (Map.elems (clFields base) ++ Map.elems (clFields cl))
            in  if null dups then Right flds else redefFldsError dups
          combinedMthds =
            let subMthds = Map.fromList $ map (\m -> (mthdName m, m)) (clMethods cl)
            in run (clMethods base) subMthds
            where
                run :: [Method Code] -> Map.Map Ident (Method Code) -> Either String [Method Code]
                run [] subMthds = return $ Map.elems subMthds
                run (b:bs) subMthds =
                    let key = mthdName b
                    in case Map.lookup key subMthds of
                           Nothing -> run bs subMthds >>= (\bs' -> return $ b : bs')
                           Just m  -> do
                               let bt = mthdTypeIgnSelf b
                                   mt = mthdTypeIgnSelf m
                               if bt == mt then run bs (Map.delete key subMthds) >>= (\bs' -> return $ m : bs')
                                           else redefMthdError b m

-- Mostly used for debugging purposes.
instance Show (Class a) where
    show (Class (Ident name) base flds mthds) = intercalate "\n" (header : map indent (fields ++ methods))
        where
            header = ".class " ++ name ++ extends ++ ":"
            extends = case base of
                Nothing    -> ""
                Just clExt -> let Ident x = clName clExt in " extends " ++ x
            fields = ".fields:" : map (indent . show) (Map.elems flds)
            methods = ".methods:" : map (indent . show) mthds
            indent x = "  " ++ x

instance Show (Method a) where
    show mthd = showType (mthdType mthd) ++ " " ++ showI (mthdName mthd) ++ ";"

instance Show Field where
    show (Fld i typ _) = showType typ ++ " " ++ showI i ++ ";"

showType :: Type a -> String
showType typ = case typ of
    Int _             -> "int"
    Str _             -> "string"
    Bool _            -> "boolean"
    Void _            -> "void"
    Var _             -> "var"
    Arr _ t           -> showType t ++ "[]"
    Cl _ (Ident name) -> name
    Fun _ t ts        -> showType t ++ "(" ++ intercalate ", " (map showType ts) ++ ")"
    Ref _ t           -> showType t

-- Errors

redefFldsError :: [Field] -> Either String a
redefFldsError flds = Left $ intercalate "\n" (header : ctxs)
    where header = "Conflicting field definitions in subclass: `" ++ intercalate "`, `" (dedup $ map (showI . fldName) flds) ++ "`."
          ctxs = sort $ map ctx flds
          ctx fld = lineInfo (codePos $ unwrap $ fldCode fld)

redefMthdError :: Method Code -> Method Code -> Either String a
redefMthdError base override = Left $ header ++ ctx
    where header = "Overriding method `" ++ showI (mthdName base)
            ++ "` has a different type than the base method.\nBase method type is `" ++ showType (mthdTypeIgnSelf base)
            ++ "`, overriding method type is `" ++ showType (mthdTypeIgnSelf override) ++ "`.\n"
          ctx = lineInfo (codePos $ mthdCode override)

dupFldsError :: [Field] -> Either String a
dupFldsError flds = Left $ intercalate "\n" (header : ctxs)
    where header = "Duplicate field identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . fldName) flds) ++ "`."
          ctxs = sort $ map ctx flds
          ctx fld = lineInfo (codePos $ unwrap $ fldCode fld)

dupMthdsError :: [Method a] -> Either String b
dupMthdsError mthds = Left $ intercalate "\n" (header : ctxs)
    where header = "Duplicate method identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . mthdName) mthds) ++ "`."
          ctxs = sort $ map ctx mthds
          ctx mthd = lineInfo (pos $ mthdCode mthd)

conflFldsAndMthdsError :: [(Field, Method a)] -> Either String b
conflFldsAndMthdsError confls = Left $ intercalate "\n" (header : ctxs)
    where header = "Conflicting field/method identifiers: `" ++ intercalate "`, `" (map (showI . fldName . fst) confls) ++ "`."
          ctxs = sort $ map ctx confls
          ctx (fld, mthd) = lineInfo (codePos $ unwrap $ fldCode fld) ++ " conflicting with " ++ lineInfo (pos $ mthdCode mthd)

reservedFldError :: [Field] -> Either String a
reservedFldError flds = Left $ intercalate "\n" (header : ctxs)
    where header = "Reserved names used as field identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . fldName) flds) ++ "`."
          ctxs = sort $ map ctx flds
          ctx fld = lineInfo (codePos $ unwrap $ fldCode fld)

reservedMthdsError :: [Method a] -> Either String b
reservedMthdsError mthds = Left $ intercalate "\n" (header : ctxs)
    where header = "Reserved names used as method identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . mthdName) mthds) ++ "`."
          ctxs = sort $ map ctx mthds
          ctx mthd = lineInfo (pos $ mthdCode mthd)

dupArgsError :: [String] -> Either String a
dupArgsError args = Left $ "Duplicate formal parameter identifiers: `" ++ intercalate "`, `" args ++ "`."
