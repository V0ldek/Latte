-- Reserved identifiers used internally by the compiler.
-- Any identifier starting with '~' is meant to be invisible
-- by user code and unspeakable using lexical rules of the language.
module Identifiers where

import           Syntax.Abs

class ToString a where
    toStr :: a -> String

instance ToString Ident where
    toStr (Ident s) = s

-- Identifier of the class that wraps toplevel functions.
topLevelClassIdent :: Ident
topLevelClassIdent = Ident "~cl_TopLevel"

-- Internal identifier of the method being currently compiled.
currentMthdSymIdent :: Ident
currentMthdSymIdent = Ident "~mthd_current"

-- Identifiers used in for loop translation.
forArrayIdent :: Ident
forArrayIdent = Ident "~l_arr"

forIndexIdent :: Ident
forIndexIdent = Ident "~l_idx"


selfSymIdent :: Ident
selfSymIdent = Ident "self"

mainSymIdent :: Ident
mainSymIdent = Ident "main"

arrayLengthIdent :: Ident
arrayLengthIdent = Ident "length"

entryLabel :: String
entryLabel = "L_entry"

reservedNames :: [Ident]
reservedNames = [selfSymIdent]
