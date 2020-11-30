module Error where

import           Syntax.Abs     (Pos, Positioned (..))
import           Syntax.Printer (Print, printTree)

errorMsg :: String -> Pos -> String -> String
errorMsg msg a ctx = msg ++ "\n" ++ ctxMsg
    where ctxMsg = lineInfo a ++ ":\n" ++ ctx

errorCtxMsg :: (Positioned a, Print a) => String -> a -> String
errorCtxMsg msg ctx = errorMsg msg (pos ctx) (printTree ctx)

lineInfo :: Pos -> String
lineInfo (ln, ch) = "Line " ++ show ln ++ ", character " ++ show ch
