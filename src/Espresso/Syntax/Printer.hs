{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
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

pattern Label :: [Char] -> [Char]
pattern Label l <- l@('.':'L':_)

-- Custom edits for better printing.
render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    ".method":ts -> new i . space ".method" . rend i ts
    "[":"]":"]":ts -> space "[" . showChar ']' . rend i ("]":ts)
    "[" : "]":ts -> space "[" . showChar ']' . new i . rend i ts
    "[":Label l:ts -> space "[" . new i . rend i (l:ts)
    "["      :ts -> showChar '[' . new (i+1) . rend (i+1) ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    t   : ";":ts -> showString t . rend i (";":ts)
    ";" : "]":ts -> showChar ';' . rend i ("]":ts)
    ";":Label l:ts -> showChar ';' . new (i-1) . rend (i-1) (l:ts)
    ";" :"/*":ts -> space ";" . space "/*" . rend i ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    ":" : "=":ts -> showChar ':' . showChar '=' . rend (i+1) ts
    "*/": "]":ts -> showString "*/" . rend i ("]":ts)
    "*/":Label l:ts -> showString "*/" . new (i-1) . rend (i-1) (l:ts)
    "*/"     :ts -> showString "*/" . new i . rend i ts
    Label l1:":":Label l2:":":ts -> showString l1 . showChar ':' . new i . rend i (l2:":":ts)
    Label l:":":"]":ts -> showString l . showChar ':' . rend (i+1) ("]":ts)
    Label l:":":ts -> showString l . showChar ':' . new (i+1) . rend (i+1) ts
    t:":":"[":ts -> showString t . space ":" . rend i ("[":ts)
    t   : ":":ts -> showString t . showChar ':' . new i . rend i ts
    ":"      :ts -> showChar ':' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    "]": "]" :ts -> new (i-1) . showChar ']' . rend (i-1) ("]":ts)
    "]"      :ts -> new (i-1) . showChar ']' . new (i-1) . rend (i-1) ts
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



instance Print (QIdent a) where
  prt i e = case e of
    QIdent _ (SymIdent s1) (SymIdent s2) -> prPrec i 0 (doc (showString s1 . showString "." . showString s2))

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
    Mthd _ stype qident params instrs -> prPrec i 0 (concatD [doc (showString ".method"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 params, doc (showString ")"), doc (showString ":"), doc (showString "["), prt 0 instrs, doc (showString "]")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (Param a) where
  prt i e = case e of
    Param _ stype valident -> prPrec i 0 (concatD [prt 0 stype, prt 0 valident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Instr a) where
  prt i e = case e of
    ILabel _ labident -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":")])
    ILabelAnn _ (LabIdent ident) n1 n2 -> prPrec i 0 (concatD [doc (showString ident), doc (showString ":"), doc (showString "/*"), doc (showString "lines"), prt 0 n1, doc (showString "to"), prt 0 n2, doc (showString "*/")])
    IVRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    IRet _ val -> prPrec i 0 (concatD [doc (showString "return"), prt 0 val, doc (showString ";")])
    IOp _ valident val1 op val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val1, prt 0 op, prt 0 val2, doc (showString ";")])
    ISet _ valident val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val, doc (showString ";")])
    IStr _ valident str -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 str, doc (showString ";")])
    IUnOp _ valident unop val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 unop, prt 0 val, doc (showString ";")])
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
    PhiVar _ (LabIdent l) val -> prPrec i 0 (concatD [doc (showString l . showChar ':'), prt 0 val])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Call a) where
  prt i e = case e of
    Call _ stype qident vals -> prPrec i 0 (concatD [doc (showString "call"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])
    CallVirt _ stype qident vals -> prPrec i 0 (concatD [doc (showString "callvirt"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])

instance Print (Val a) where
  prt i e = case e of
    VInt _ n -> prPrec i 0 (concatD [prt 0 n])
    VNegInt _ n -> prPrec i 0 (concatD [doc (showString "-"), prt 0 n])
    VTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    VFalse _ -> prPrec i 0 (concatD [doc (showString "false")])
    VNull _ -> prPrec i 0 (concatD [doc (showString "null")])
    VVal _ stype valident -> prPrec i 0 (concatD [prt 0 stype, prt 0 valident])
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

instance Print (UnOp a) where
  prt i e = case e of
    UnOpNeg _ -> prPrec i 0 (concatD [doc (showString "-")])
    UnOpNot _ -> prPrec i 0 (concatD [doc (showString "!")])

printTreeWithInstrComments :: (Show a) => Program a -> String
printTreeWithInstrComments = render . prtWComments 0 

class PrintWithComments a where
  prtWComments :: Int -> a -> Doc
  prtListWComments :: Int -> [a] -> Doc
  prtListWComments i = concatD . map (prtWComments i)

instance PrintWithComments a => PrintWithComments [a] where
  prtWComments = prtListWComments

instance PrintWithComments Char where
  prtWComments = prt

instance PrintWithComments Integer where
  prtWComments = prt

instance PrintWithComments Double where
  prtWComments = prt

instance PrintWithComments SymIdent where
  prtWComments = prt

instance PrintWithComments LabIdent where
  prtWComments = prt

instance PrintWithComments ValIdent where
  prtWComments = prt

instance PrintWithComments (QIdent a) where
  prtWComments = prt

instance Show a => PrintWithComments (Program a) where
  prtWComments i e = case e of
    Program _ metadata methods -> prPrec i 0 (concatD [prtWComments 0 metadata, prtWComments 0 methods])

instance PrintWithComments (Metadata a) where
  prtWComments = prt
  
instance PrintWithComments (ClassDef a) where
  prtWComments = prt

instance PrintWithComments (FieldDef a) where
  prtWComments = prt

instance PrintWithComments (MethodDef a) where
  prtWComments = prt

instance PrintWithComments (FType a) where
  prtWComments = prt

instance PrintWithComments (SType a) where
  prtWComments = prt

instance Show a => PrintWithComments (Method a) where
  prtWComments i e = case e of    
    Mthd _ stype qident params instrs -> prPrec i 0 (concatD [doc (showString ".method"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 params, doc (showString ")"), doc (showString ":"), doc (showString "["), prtWComments 0 instrs, doc (showString "]")])
  prtListWComments _ [] = (concatD [])
  prtListWComments _ (x:xs) = (concatD [prtWComments 0 x, prtWComments 0 xs])

instance Show a => PrintWithComments (Instr a) where
  prtWComments i e = case e of
    ILabel a labident -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    ILabelAnn a (LabIdent ident) n1 n2 -> prPrec i 0 (concatD [doc (showString ident), doc (showString ":"), doc (showString "/*"), doc (showString "lines"), prt 0 n1, doc (showString "to"), prt 0 n2, doc (showString "*/"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IVRet a -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IRet a val -> prPrec i 0 (concatD [doc (showString "return"), prt 0 val, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IOp a valident val1 op val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val1, prt 0 op, prt 0 val2, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    ISet a valident val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IStr a valident str -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 str, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IUnOp a valident unop val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 unop, prt 0 val, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IVCall a call -> prPrec i 0 (concatD [prt 0 call, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    ICall a valident call -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 call, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IJmp a labident -> prPrec i 0 (concatD [doc (showString "jump"), prt 0 labident, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    ICondJmp a val labident1 labident2 -> prPrec i 0 (concatD [doc (showString "jump"), doc (showString "if"), prt 0 val, doc (showString "then"), prt 0 labident1, doc (showString "else"), prt 0 labident2, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    ILoad a valident val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "load"), prt 0 val, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IStore a val1 val2 -> prPrec i 0 (concatD [doc (showString "store"), prt 0 val1, prt 0 val2, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IFld a valident val qident -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "fldptr"), prt 0 val, prt 0 qident, doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IArr a valident val1 val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "elemptr"), prt 0 val1, doc (showString "["), prt 0 val2, doc (showString "]"), doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
    IPhi a valident phivariants -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "phi"), doc (showString "("), prt 0 phivariants, doc (showString ")"), doc (showString ";"), doc (showString "/*"), doc (shows a), doc (showString "*/")])
  prtListWComments _ [] = (concatD [])
  prtListWComments _ (x:xs) = (concatD [prtWComments 0 x, prtWComments 0 xs])

instance PrintWithComments (PhiVariant a) where
  prtWComments = prt

instance PrintWithComments (Call a) where
  prtWComments = prt

instance PrintWithComments (Val a) where
  prtWComments = prt

instance PrintWithComments (Op a) where
  prtWComments = prt

instance PrintWithComments (UnOp a) where
  prtWComments = prt