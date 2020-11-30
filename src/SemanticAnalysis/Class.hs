module SemanticAnalysis.Class (
    Class (..),
    Field (..),
    Method (..),
    MethodCode (..),
    mthdBody,
    mthdType,
    topLevelClassIdent,
    clCons,
    fldCons,
    funCons,
    mthdCons,
    clExtend
) where

import           Control.Monad
import           Data.List     (intercalate, sort)
import qualified Data.Map      as Map
import           Error
import           Syntax.Abs

data Class = Class { clName :: Ident, clBase :: Maybe Class, clFields :: [Field], clMethods :: [Method] }

data Field = Fld { fldName :: Ident, fldType :: Type (), fldCode :: ClDef Pos}

data Method = Mthd {
     mthdName :: Ident,
     mthdRet  :: Type (),
     mthdThis :: Maybe (Type ()),
     mthdArgs :: [Arg ()],
     mthdCode :: MethodCode }

data MethodCode = FnCode (TopDef Pos) | MthdCode (ClDef Pos)

instance Positioned MethodCode where
    pos x = case x of
        FnCode td   -> unwrap td
        MthdCode cl -> unwrap cl

mthdBody :: Method -> Block Pos
mthdBody x = case mthdCode x of
    FnCode (FnDef _ _ _ _ blk)    -> blk
    MthdCode (MthDef _ _ _ _ blk) -> blk

mthdType :: Method -> Type ()
mthdType mthd = case mthdThis mthd of
    Nothing  -> mthdTypeIgnThis mthd
    Just typ -> let Fun _ ret args = mthdTypeIgnThis mthd
                in  Fun () ret (typ : args)

mthdTypeIgnThis :: Method -> Type ()
mthdTypeIgnThis (Mthd _ ret _ args _) = Fun () ret (map (\(Arg _ t _) -> t) args)

topLevelClassIdent :: Ident
topLevelClassIdent = Ident "~cl_TopLevel"

clCons :: Ident -> Maybe Class -> [Field] -> [Method] -> Either String Class
clCons name base flds mthds = do
    let dupFlds = snd $ findDupsBy (showI . fldName) flds
        dupMthds = snd $ findDupsBy (showI . mthdName) mthds
    unless (null dupFlds) (dupFldsError dupFlds)
    unless (null dupMthds) (dupMthdsError dupMthds)
    return $ Class name base flds mthds

dupFldsError :: [Field] -> Either String a
dupFldsError flds = Left $ intercalate "\n" (header : ctxs)
    where header = "Duplicate field identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . fldName) flds) ++ "`."
          ctxs = sort $ map ctx flds
          ctx fld = lineInfo (unwrap $ fldCode fld)

dupMthdsError :: [Method] -> Either String a
dupMthdsError mthds = Left $ intercalate "\n" (header : ctxs)
    where header = "Duplicate method identifiers: `" ++ intercalate "`, `" (dedup $ map (showI . mthdName) mthds) ++ "`."
          ctxs = sort $ map ctx mthds
          ctx mthd = lineInfo (pos $ mthdCode mthd)

fldCons :: Type a -> Ident -> ClDef Pos -> Field
fldCons typ i = Fld i (() <$ typ)

funCons :: Type a -> Ident -> [Arg c] -> TopDef Pos -> Either String Method
funCons typ i args code = do
    cons <- baseMethodCons typ i Nothing args
    return $ cons $ FnCode code

mthdCons :: Type a -> Ident -> Type b -> [Arg c] -> ClDef Pos -> Either String Method
mthdCons typ i thisTyp args code = do
    cons <- baseMethodCons typ i (Just thisTyp) args
    return $ cons $ MthdCode code

baseMethodCons :: Type a -> Ident -> Maybe (Type b) -> [Arg c] -> Either String (MethodCode -> Method)
baseMethodCons typ i thisTyp args = do
    let dupArgs = fst $ findDupsBy (\(Arg _ _ i) -> showI i) args
    unless (null dupArgs) (dupArgsError dupArgs)
    return $ Mthd i (() <$ typ) (fmap (() <$) thisTyp) (map (() <$) args)

dupArgsError :: [String] -> Either String a
dupArgsError args = Left $ "Duplicate formal parameter identifiers: `" ++ intercalate "`, `" args ++ "`."

clExtend :: Class -> Class -> Either String Class
clExtend cl base = do
    flds <- combinedFlds
    return $ Class (clName cl) (Just base) flds combinedMthds
    where combinedFlds = let flds = clFields base ++ clFields cl
                             (_, dups) = findDupsBy fldName flds
                         in  if null dups then Right flds else redefFldsError dups
          combinedMthds = let subMthds = Map.fromList $ map (\m -> ((mthdName m, mthdTypeIgnThis m), m)) (clMethods cl)
                         in run (clMethods base) subMthds
                         where
                            run [] subMthds = Map.elems subMthds
                            run (b:bs) subMthds = let key = (mthdName b, mthdTypeIgnThis b)
                                                  in case Map.lookup key subMthds of
                                                      Nothing -> b : run bs subMthds
                                                      Just m  -> m : run bs (Map.delete key subMthds)

redefFldsError :: [Field] -> Either String a
redefFldsError flds = Left $ intercalate "\n" (header : ctxs)
    where header = "Conflicting field definitions in subclass: `" ++ intercalate "`, `" (dedup $ map (showI . fldName) flds) ++ "`."
          ctxs = sort $ map ctx flds
          ctx fld = lineInfo (unwrap $ fldCode fld)

-- TODO: better error message
redefMthdsError :: [Method] -> Either String a
redefMthdsError mthds = Left $ intercalate "\n" (header : ctxs)
    where header = "Invalid type of virtual method overload in subclass: `" ++ intercalate "`, `" (dedup $ map (showI . mthdName) mthds) ++ "`."
          ctxs = sort $ map ctx mthds
          ctx mthd = lineInfo (pos $ mthdCode mthd)

-- Mostly used for debugging purposes.
instance Show Class where
    show (Class (Ident name) base clFields clMethods) = intercalate "\n" (header : map indent (fields ++ methods))
        where
            header = ".class " ++ name ++ extends ++ ":"
            extends = case base of
                Nothing    -> ""
                Just clExt -> let Ident x = clName clExt in " extends " ++ x
            fields = ".fields:" : map (indent . show) clFields
            methods = ".methods:" : map (indent . show) clMethods
            indent x = "  " ++ x

instance Show Method where
    show mthd = showType (mthdType mthd) ++ " " ++ showI (mthdName mthd) ++ ";"

instance Show Field where
    show (Fld i typ _) = showType typ ++ " " ++ showI i ++ ";"

showType :: Type a -> String
showType typ = case typ of
    Int _             -> "int"
    Str _             -> "string"
    Bool _            -> "boolean"
    Void _            -> "void"
    Arr _ t           -> showType t ++ "[]"
    Cl _ (Ident name) -> name
    Fun _ typ typs    -> showType typ ++ "(" ++ intercalate ", " (map showType typs) ++ ")"
    Ref _ t           -> showType t ++ "&"

findDupsBy :: Ord k => (a -> k) -> [a] -> ([k], [a])
findDupsBy f ds = collect $ foldr checkForDup (Map.empty, []) ds
 where
  checkForDup a (m, dups) =
    let k = f a
    in  if Map.member k m then (m, (k, a) : dups) else (Map.insert k a m, dups)
  collect (m, dups) =
    let (ks, as) = unzip dups in (ks, foldr (\k as' -> m Map.! k : as') as ks)

dedup :: Ord a => [a] -> [a]
dedup xs = run (sort xs)
    where run []     = []
          run (x:xs) = x : run (dropWhile (== x) xs )
