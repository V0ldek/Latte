-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrM where

-- the Error monad: like Maybe type with error msgs

import           Control.Applicative (Alternative (..))
import           Control.Monad       (MonadPlus (..), liftM)

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance MonadFail Err where
  fail        = Bad

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o


instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

toEither :: Err a -> Either String a
toEither e = case e of
  Ok a  -> Right a
  Bad s -> Left s
