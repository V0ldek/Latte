module Espi where

import           Control.Monad            (when)
import           System.Environment       (getArgs)
import           System.Exit              (ExitCode (..), exitFailure, exitWith)
import           System.IO                (hPutStr, hPutStrLn, stderr)

import           ErrM                     (toEither)
import           Espresso.ControlFlow.CFG
import           Espresso.Interpreter     (interpret)
import           Espresso.Syntax.Abs
import           Espresso.Syntax.Lexer    (Token)
import           Espresso.Syntax.Parser   (myLexer, pProgram)
import           Espresso.Syntax.Printer  (Print, printTree)

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
      let tree'@(Program a meta mthds) = unwrapPos tree
      putStrErrLn "OK"
      showTree v tree'
      let cfgs = zip (map cfg mthds) mthds
      showCfgs v cfgs
      let mthds' = map (\(g, Mthd a' i _) -> Mthd a' i (linearize g)) cfgs
          tree'' = Program a meta mthds'
      showTree v tree''
      n <- interpret tree''
      exitWith (if n == 0 then ExitSuccess else ExitFailure n)
  where
  ts = myLexer s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

showCfgs :: Int -> [(CFG a, Method a)] -> IO ()
showCfgs v = mapM_ showCfg
  where
    showCfg (g, Mthd _ (QIdent _ (SymIdent i1) (SymIdent i2)) _) = do
      putStrV v ("CFG for " ++ i1 ++ "." ++ i2 ++ ":")
      putStrV v $ show g

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Espresso interpreter.",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  (file)         Interpret content of the file.",
        "  -v (file)      Verbose mode. Interpret content of the file verbosely."
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
