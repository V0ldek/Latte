module Latc_x86_64 where

import           Compiler           (Options (Opt), Verbosity (..),
                                     assemblyFile, run)
import           Control.Monad
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

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
        [f] -> do
          run (Opt f generateIntermediate verbosity)
          let fileName = dropExtension $ takeFileName f
              directory = takeDirectory f
              outputPath = directory </> fileName
              asmPath = assemblyFile directory fileName
          when (verbosity == Verbose) $ putStrLn "Compiling with gcc..."
          exitCode <- runCommand (gccCommand runtimePath asmPath outputPath) >>= waitForProcess
          unless (exitCode == ExitSuccess) (failGcc exitCode)
          when (verbosity == Verbose) $ putStrLn "Success."
          exitSuccess
        _   -> usage

gccCommand :: FilePath -> FilePath -> FilePath -> String
gccCommand libPath assemblyPath outputPath =
  "gcc " ++ show libPath ++ " " ++ assemblyPath ++ " -o " ++ outputPath

runtimePath :: FilePath
runtimePath = "." </> "lib" </> "runtime.o"

failGcc :: ExitCode -> IO ()
failGcc c = putStr "GCC failed with exit code " >> print c >> exitFailure
