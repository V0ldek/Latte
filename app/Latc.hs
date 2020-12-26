module Latc where

import           System.Environment         (getArgs)
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hPutStr, hPutStrLn, stderr)

import           Control.Monad              (when)
import           ErrM                       (toEither)
import           Espresso.CodeGen.Generator (generateEspresso)
import           Espresso.Syntax.Printer    as PrintEspresso (Print, printTree)
import           SemanticAnalysis.Analyser  (analyse)
import           SemanticAnalysis.TopLevel  (Metadata, programMetadata)
import           Syntax.Abs                 (Pos, Program, unwrapPos)
import           Syntax.Lexer               (Token)
import           Syntax.Parser              (myLexer, pProgram)
import           Syntax.Printer             as PrintLatte (Print, printTree)
import           Syntax.Rewriter            (rewrite)

type Err = Either String
type ParseResult = (Program (Maybe Pos))

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

runFile :: Verbosity -> ParseFun ParseResult -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

run :: Verbosity -> ParseFun ParseResult -> String -> IO ()
run v p s = case p ts of
    Left err -> do
      putStrErrLn "ERROR"
      putStrErrV v "Tokens:"
      putStrErrV v $ show ts
      putStrErrLn err
      exitFailure
    Right tree -> do
      let tree' = unwrapPos tree
      putStrErrLn "OK"
      showTree v tree'
      let rewritten = rewrite tree'
      putStrErrV v "Rewritten:"
      showTree v rewritten
      () <- case programMetadata rewritten of
        Left err -> do
          putStrErrLn "ERROR"
          putStrErrLn err
          exitFailure
        Right meta -> do
          showMetadata v meta
          case analyse meta of
            Right meta' -> do
              let esp = generateEspresso meta'
              showEspressoTree v esp
              exitSuccess
            Left err -> do
              putStrErrLn "ERROR"
              putStrErrLn err
              exitFailure
      exitSuccess
  where
  ts = myLexer s

showMetadata :: Verbosity -> Metadata a -> IO ()
showMetadata v meta = putStrV v $ show meta

showTree :: (Show a, PrintLatte.Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ PrintLatte.printTree tree

showEspressoTree :: (Show a, PrintEspresso.Print a) => Int -> a -> IO ()
showEspressoTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ PrintEspresso.printTree tree

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
