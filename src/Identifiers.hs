-- Reserved identifiers used internally by the compiler.
-- Any identifier starting with '~' is meant to be invisible
-- by user code and unspeakable using lexical rules of the language.
module Identifiers where

import           Espresso.Syntax.Abs
import           Syntax.Abs

class ToString a where
    toStr :: a -> String

instance ToString Ident where
    toStr (Ident s) = s

instance ToString LabIdent where
    toStr (LabIdent s) = s

instance ToString SymIdent where
    toStr (SymIdent s) = s

instance ToString ValIdent where
    toStr (ValIdent s) = s

argValIdent :: String -> ValIdent
argValIdent s = ValIdent $ "%a_" ++ s

arrayLengthIdent :: Ident
arrayLengthIdent = Ident "length"

constIdent :: String -> String
constIdent = ("__const_" ++)

-- Internal identifier of the method being currently compiled.
currentMthdSymIdent :: Ident
currentMthdSymIdent = Ident "~mthd_current"

entryLabel :: LabIdent
entryLabel = LabIdent ".L_entry"

exitLabel :: LabIdent
exitLabel = LabIdent ".L_exit"

-- Identifiers used in for loop translation.
forArrayIdent :: Ident
forArrayIdent = Ident "~v_arr"

forIndexIdent :: Ident
forIndexIdent = Ident "~v_idx"

indexedValIdent :: String -> Integer -> ValIdent
indexedValIdent i idx =
    let suf = if idx == 0 then "" else '_':show idx
    in valIdent (i ++ suf)

labIdent :: String -> LabIdent
labIdent = LabIdent . (".L_" ++)

labelFor :: QIdent a -> LabIdent -> LabIdent
labelFor (QIdent _ (SymIdent i1) (SymIdent i2)) (LabIdent l1) = LabIdent $ i1 ++ "." ++ i2 ++ l1

mainSymIdent :: Ident
mainSymIdent = Ident "main"

phiUnfoldJumpFromToLabel :: LabIdent -> LabIdent -> LabIdent
phiUnfoldJumpFromToLabel (LabIdent from) (LabIdent to) = LabIdent $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs

reservedNames :: [Ident]
reservedNames = [selfSymIdent]

sanitiseAssembly :: String -> String
sanitiseAssembly s = case s of
    []     -> []
    '~':xs -> '_':'_':sanitiseAssembly xs
    x:xs   -> x:sanitiseAssembly xs

selfSymIdent :: Ident
selfSymIdent = Ident "self"

-- Identifier of the class that wraps top level functions.
topLevelClassIdent :: Ident
topLevelClassIdent = Ident "~cl_TopLevel"

valIdent :: String -> ValIdent
valIdent = ValIdent . ("%v_" ++)

runtimeSymbols :: [String]
runtimeSymbols = [
        "lat_print_int",
        "lat_print_string",
        "lat_read_int",
        "lat_read_string",
        "lat_error",
        "lat_nullchk",
        "lat_new_string",
        "lat_cat_strings"
    ]

getCallTarget :: QIdent a -> String
getCallTarget (QIdent _ (SymIdent i1) (SymIdent i2)) =
    if i1 == toStr topLevelClassIdent
    then case i2 of
        "readInt"     -> "lat_read_int"
        "readString"  -> "lat_read_string"
        "printInt"    -> "lat_print_int"
        "printString" -> "lat_print_string"
        "error"       -> "lat_error"
        _             -> i1 ++ "." ++ i2
    else i1 ++ "." ++ i2
