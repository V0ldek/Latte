module Main where

import           Control.Monad      (unless, when)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hPutStr, hPutStrLn, stderr)

import           ErrM               (toEither)
import           Syntax.Lexer       (Token)
import           Syntax.Parser      (myLexer, pProgram)
import           Syntax.Printer     (Print, printTree)

type Err = Either String

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

putStrErrV :: Verbosity -> String -> IO ()
putStrErrV v s = when (v > 1) $ hPutStrLn stderr s

putStrErr :: String -> IO ()
putStrErr = hPutStr stderr

putStrErrLn :: String -> IO ()
putStrErrLn = hPutStrLn stderr

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = do
  b <- p
  unless b a

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = case p ts of
    Left err -> do
      putStrErrLn "ERROR"
      putStrErrV v "Tokens:"
      putStrErrV v $ show ts
      putStrErrLn err
      exitFailure
    Right tree -> do
      putStrErrLn "OK"
      showTree v tree
      exitSuccess
  where
  ts = myLexer s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Latte compiler.",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  (file)         Compile content of the file.",
        "  -v (file)      Verbose mode. Compile content of the file verbosely."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-v" : [f] -> runFile 2 (toEither . pProgram) f
    [f]        -> runFile 0 (toEither . pProgram) f
    _          -> usage
