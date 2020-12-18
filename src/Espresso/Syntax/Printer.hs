{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Espresso.Syntax.Printer where

-- pretty-printer generated by the BNF converter

import Espresso.Syntax.Abs
import Data.Char

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print SymIdent where
  prt _ (SymIdent i) = doc (showString ( i))


instance Print LabIdent where
  prt _ (LabIdent i) = doc (showString ( i))


instance Print ValIdent where
  prt _ (ValIdent i) = doc (showString ( i))


instance Print ArgIdent where
  prt _ (ArgIdent i) = doc (showString ( i))



instance Print (QIdent a) where
  prt i e = case e of
    QIdent _ symident1 symident2 -> prPrec i 0 (concatD [prt 0 symident1, doc (showString "."), prt 0 symident2])

instance Print (Program a) where
  prt i e = case e of
    Program _ metadata methods -> prPrec i 0 (concatD [prt 0 metadata, prt 0 methods])

instance Print (Metadata a) where
  prt i e = case e of
    Meta _ classdefs -> prPrec i 0 (concatD [doc (showString ".metadata"), doc (showString ":"), doc (showString "["), doc (showString ".classes"), doc (showString ":"), doc (showString "["), prt 0 classdefs, doc (showString "]"), doc (showString "]")])

instance Print (ClassDef a) where
  prt i e = case e of
    ClDef _ symident fielddefs methoddefs -> prPrec i 0 (concatD [prt 0 symident, doc (showString ":"), doc (showString "["), doc (showString ".fields"), doc (showString ":"), doc (showString "["), prt 0 fielddefs, doc (showString "]"), doc (showString ".methods"), doc (showString ":"), doc (showString "["), prt 0 methoddefs, doc (showString "]"), doc (showString "]")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (FieldDef a) where
  prt i e = case e of
    FldDef _ stype symident -> prPrec i 0 (concatD [prt 0 stype, prt 0 symident])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print (MethodDef a) where
  prt i e = case e of
    MthdDef _ ftype qident -> prPrec i 0 (concatD [prt 0 ftype, prt 0 qident])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print (FType a) where
  prt i e = case e of
    FType _ stype stypes -> prPrec i 0 (concatD [prt 0 stype, doc (showString "("), prt 0 stypes, doc (showString ")")])

instance Print (SType a) where
  prt i e = case e of
    Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    Arr _ stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "[]")])
    Cl _ symident -> prPrec i 0 (concatD [prt 0 symident])
    Ref _ stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "&")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Method a) where
  prt i e = case e of
    Mthd _ qident instrs -> prPrec i 0 (concatD [doc (showString ".method"), prt 0 qident, doc (showString ":"), doc (showString "["), prt 0 instrs, doc (showString "]")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Instr a) where
  prt i e = case e of
    ILabel _ labident -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":")])
    ILabelAnn _ labident n1 n2 n3 n4 -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":"), doc (showString "--"), doc (showString "lines"), prt 0 n1, doc (showString "-"), prt 0 n2, doc (showString "chars"), prt 0 n3, doc (showString "-"), prt 0 n4])
    IVRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    IRet _ val -> prPrec i 0 (concatD [doc (showString "return"), prt 0 val, doc (showString ";")])
    IOp _ valident val1 op val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val1, prt 0 op, prt 0 val2, doc (showString ";")])
    IVCall _ call -> prPrec i 0 (concatD [prt 0 call, doc (showString ";")])
    ICall _ valident call -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 call, doc (showString ";")])
    IJmp _ labident -> prPrec i 0 (concatD [doc (showString "jump"), prt 0 labident, doc (showString ";")])
    ICondJmp _ val labident1 labident2 -> prPrec i 0 (concatD [doc (showString "jump"), doc (showString "if"), prt 0 val, doc (showString "then"), prt 0 labident1, doc (showString "else"), prt 0 labident2, doc (showString ";")])
    ILoad _ valident val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "load"), prt 0 val, doc (showString ";")])
    IStore _ val1 val2 -> prPrec i 0 (concatD [doc (showString "store"), prt 0 val1, prt 0 val2, doc (showString ";")])
    IFld _ valident val qident -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "fldptr"), prt 0 val, prt 0 qident, doc (showString ";")])
    IArr _ valident val1 val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "elemptr"), prt 0 val1, doc (showString "["), prt 0 val2, doc (showString "]"), doc (showString ";")])
    IPhi _ valident phivariants -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "phi"), doc (showString "("), prt 0 phivariants, doc (showString ")"), doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (PhiVariant a) where
  prt i e = case e of
    PhiVar _ labident val -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":"), prt 0 val])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Call a) where
  prt i e = case e of
    Call _ qident vals -> prPrec i 0 (concatD [doc (showString "call"), prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])
    CallVirt _ qident vals -> prPrec i 0 (concatD [doc (showString "callvirt"), prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])

instance Print (Val a) where
  prt i e = case e of
    VInt _ n -> prPrec i 0 (concatD [prt 0 n])
    VStr _ str -> prPrec i 0 (concatD [prt 0 str])
    VTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    VFalse _ -> prPrec i 0 (concatD [doc (showString "false")])
    VNull _ -> prPrec i 0 (concatD [doc (showString "null")])
    VVal _ valident -> prPrec i 0 (concatD [prt 0 valident])
    VArg _ argident -> prPrec i 0 (concatD [prt 0 argident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Op a) where
  prt i e = case e of
    OpAdd _ -> prPrec i 0 (concatD [doc (showString "+")])
    OpSub _ -> prPrec i 0 (concatD [doc (showString "-")])
    OpMul _ -> prPrec i 0 (concatD [doc (showString "*")])
    OpDiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    OpMod _ -> prPrec i 0 (concatD [doc (showString "%")])
    OpLTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    OpLE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    OpGTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    OpGE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    OpEQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    OpNE _ -> prPrec i 0 (concatD [doc (showString "!=")])
    OpAnd _ -> prPrec i 0 (concatD [doc (showString "&")])
    OpOr _ -> prPrec i 0 (concatD [doc (showString "|")])


