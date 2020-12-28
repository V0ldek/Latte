module Latc where

import           Compiler           (Options (Opt), Verbosity (..), run)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

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
  if args == ["--help"] then usage
  else do
    let verbosity            = if "-v" `elem` args then Verbose else Quiet
        generateIntermediate = "-g" `elem` args
        inputFile            = filter (\a -> head a /= '-') args
    case inputFile of
        [f] -> run (Opt f generateIntermediate verbosity)
        _   -> usage
