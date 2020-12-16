{-# LANGUAGE FlexibleInstances #-}
-- Representation of a piece of source code with its position info.
module Syntax.Code where

import           Syntax.Abs
import           Syntax.Printer

data Code = Code { codeString :: String, codePos :: Maybe Pos }

toCode :: (Unwrappable f, Positioned a, Print (f a)) => f a -> Code
toCode x = Code (render $ prt 0 x) (pos $ unwrap x)

instance Positioned Code where
    pos = codePos

instance Positioned (Maybe Code) where
    pos x = x >>= codePos

instance Show Code where
    show = codeString
