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

-- Identifier of the class that wraps top level functions.
topLevelClassIdent :: Ident
topLevelClassIdent = Ident "~cl_TopLevel"

-- Internal identifier of the method being currently compiled.
currentMthdSymIdent :: Ident
currentMthdSymIdent = Ident "~mthd_current"

-- Identifiers used in for loop translation.
forArrayIdent :: Ident
forArrayIdent = Ident "~v_arr"

forIndexIdent :: Ident
forIndexIdent = Ident "~v_idx"


selfSymIdent :: Ident
selfSymIdent = Ident "self"

mainSymIdent :: Ident
mainSymIdent = Ident "main"

arrayLengthIdent :: Ident
arrayLengthIdent = Ident "length"

entryLabel :: LabIdent
entryLabel = LabIdent ".L_entry"

exitLabel :: LabIdent
exitLabel = LabIdent ".L_exit"

argValIdent :: String -> ValIdent
argValIdent s = ValIdent $ "%a_" ++ s

valIdent :: String -> ValIdent
valIdent = ValIdent . ("%v_" ++)

labIdent :: String -> LabIdent
labIdent = LabIdent . (".L_" ++)

phiUnfoldJumpFromToLabel :: LabIdent -> LabIdent -> LabIdent
phiUnfoldJumpFromToLabel (LabIdent from) (LabIdent to) = LabIdent $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs

reservedNames :: [Ident]
reservedNames = [selfSymIdent]
