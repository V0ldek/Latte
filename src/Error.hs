{-# LANGUAGE FlexibleInstances #-}
module Error where

import           Data.Maybe
import           Syntax.Abs  (Pos, Positioned (..), Unwrappable (..))
import           Syntax.Code

class WithContext a where
  getCtx :: a -> String

instance WithContext Code where
  getCtx = codeString

instance WithContext (Maybe Code) where
  getCtx c = maybe "" getCtx c

errorMsg :: Positioned a => String -> a -> String -> String
errorMsg msg a ctx = msg ++ "\n" ++ ctxMsg
  where
    ctxMsg = lineInfo (pos a) ++ ":\n" ++ ctx

errorMsgMb :: Positioned a => String -> Maybe a -> Maybe String -> String
errorMsgMb msg a ctx = msg ++ "\n" ++ ctxMsg
  where
    ctxMsg = lineInfo (a >>= pos) ++ ":\n" ++ fromMaybe "" ctx

errorCtxMsg :: (Positioned a, WithContext a, Unwrappable f) => String -> f a -> String
errorCtxMsg msg ctx = errorMsg msg (unwrap ctx) (getCtx $ unwrap ctx)

lineInfo :: Maybe Pos -> String
lineInfo a = case a of
    Nothing       -> ""
    Just (ln, ch) -> "Line " ++ show ln ++ ", character " ++ show ch
