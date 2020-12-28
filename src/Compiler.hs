module Compiler where

import           Control.Monad                 (when)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import           ErrM                          (toEither)
import           Espresso.CodeGen.Generator    (generateEspresso)
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness (Liveness)
import qualified Espresso.Syntax.Abs           as Esp
import           Espresso.Syntax.Printer       as PrintEsp (Print, printTree,
                                                            printTreeWithInstrComments)
import           Identifiers                   (ToString (toStr))
import           LatteIO
import           SemanticAnalysis.Analyser     (SemData, analyse)
import           SemanticAnalysis.TopLevel     (Metadata, programMetadata)
import           Syntax.Abs                    as Latte (Pos, Program,
                                                         unwrapPos)
import           Syntax.Lexer                  (Token)
import           Syntax.Parser                 (myLexer, pProgram)
import           Syntax.Printer                as PrintLatte (Print, printTree)
import           Syntax.Rewriter               (rewrite)
import           System.FilePath               (dropExtension, takeDirectory,
                                                takeFileName, (<.>), (</>))
import           Utilities                     (single, unlessM)

data Verbosity = Quiet | Verbose deriving (Eq, Ord, Show)

data Options = Opt {
    inputFile            :: String,
    generateIntermediate :: Bool,
    verbosity            :: Verbosity
}

type Err = Either String
type ParseResult = (Program (Maybe Pos))
type ParseFun a = [Token] -> Err a

printStringV :: (Monad m, LatteIO m) => Verbosity -> String -> m ()
printStringV v s = when (v == Verbose) $ printString s

printErrorStringV :: (Monad m, LatteIO m) => Verbosity -> String -> m ()
printErrorStringV v s = when (v == Verbose) $ printErrorString s

run :: (Monad m, LatteIO m) => Options -> m ()
run opt = do
    let f = inputFile opt
        v = verbosity opt
        fileName = dropExtension $ takeFileName f
        directory = takeDirectory f
    unlessM (doesDirectoryExist directory) (failNoDirectory directory)
    unlessM (doesFileExist f) (failNoFile f)
    latSrc <- LatteIO.readFile $ inputFile opt
    printStringV v "Analysing Latte..."
    latte <- analysePhase opt latSrc
    printStringV v "Brewing Esp..."
    let espresso@(Esp.Program a meta mthds) = generateEspresso latte
    genStep opt (espressoFile directory fileName) (PrintEsp.printTree espresso)
    printStringV v "Building CFGs..."
    let cfgs = zip (map cfg mthds) mthds
        espressoOpt = PrintEsp.printTree $ Esp.Program a meta (cfgsToMthds cfgs)
    genStep opt (espressoCfgFile directory fileName) (showCfgs cfgs)
    genStep opt (espressoOptFile directory fileName) espressoOpt
    {-printStringV v "Analysing liveness..."
    let cfgsWithLiveness = map (first analyseLiveness) cfgs
        espressoWithLiveness = showEspWithLiveness meta (cfgsToMthds cfgsWithLiveness)
    genStep opt (espressoCfgWithLivenessFile directory fileName) (showCfgsWithLiveness cfgsWithLiveness)
    genStep opt (espressoOptWithLivenessFile directory fileName) espressoWithLiveness-}
    exitSuccess

analysePhase :: (Monad m, LatteIO m) => Options -> String -> m (Metadata SemData)
analysePhase opt latSrc = do
    let v = verbosity opt
        tokens = myLexer latSrc
    tree <- case toEither $ pProgram tokens of
        Left err -> do
            printErrorString "ERROR"
            printErrorStringV v "Tokens:"
            printErrorStringV v $ show tokens
            printErrorString err
            exitFailure
        Right tree -> return tree
    let tree' = unwrapPos tree
        rewritten = rewrite tree'
    meta <- case programMetadata rewritten of
        Left err -> do
            printErrorString "ERROR"
            printErrorStringV v "Rewritten:"
            showTree v rewritten
            printErrorString err
            exitFailure
        Right meta -> return meta
    meta' <- case analyse meta of
        Left err -> do
            printErrorString "ERROR"
            printErrorStringV v "Metadata:"
            showMetadata v meta
            printErrorString err
            exitFailure
        Right meta' -> return meta'
    printErrorString "OK"
    return meta'

genStep :: (Monad m, LatteIO m) => Options -> FilePath -> String -> m ()
genStep opt fp contents = do
    let v = verbosity opt
        g = generateIntermediate opt
    if not g then return ()
    else do
        printStringV v $ "Writing " ++ show fp ++ "..."
        LatteIO.writeFile fp contents

failNoDirectory :: (Monad m, LatteIO m) => FilePath -> m ()
failNoDirectory d = printErrorString ("Directory not found: " ++ show d) >> exitFailure

failNoFile :: (Monad m, LatteIO m) => FilePath -> m ()
failNoFile f = printErrorString ("File not found: " ++ show f) >> exitFailure

showMetadata :: (Monad m, LatteIO m) => Verbosity -> Metadata a -> m ()
showMetadata v meta = printStringV v $ show meta

showTree :: (Monad m, LatteIO m, Show a, PrintLatte.Print a) => Verbosity -> a -> m ()
showTree v tree
 = do
      printStringV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      printStringV v $ "\n[Linearized tree]\n\n" ++ PrintLatte.printTree tree

showEspTree :: (Monad m, LatteIO m, Show a, PrintEsp.Print a) => Verbosity -> a -> m ()
showEspTree v tree
 = do
      printStringV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      printStringV v $ "\n[Linearized tree]\n\n" ++ PrintEsp.printTree tree

showCfgs :: [(CFG a, Esp.Method a)] -> String
showCfgs cfgs = unlines $ map showCfg cfgs
  where
    showCfg (g, Esp.Mthd _ (Esp.QIdent _ (Esp.SymIdent i1) (Esp.SymIdent i2)) _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show g

cfgsToMthds :: [(CFG a, Esp.Method b)] -> [Esp.Method a]
cfgsToMthds = map (\(g, Esp.Mthd _ i _) -> Esp.Mthd undefined (undefined <$ i) (linearize g))

showCfgsWithLiveness :: [(CFG Liveness, Esp.Method a)] -> String
showCfgsWithLiveness cfgs = unlines $ map showCfg cfgs
  where
    showCfg (CFG g, Esp.Mthd _ (Esp.QIdent _ (Esp.SymIdent i1) (Esp.SymIdent i2)) _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show (CFG g) ++ concatMap showLiveness (Map.elems g)
    showLiveness node =
        let firstInstr = head $ nodeCode node
            lastInstr = last $ nodeCode node
        in "Liveness at start of " ++ toStr (nodeLabel node) ++ ": " ++ show (fromJust $ single firstInstr) ++ "\n" ++
           "Liveness at end of " ++ toStr (nodeLabel node) ++ ": " ++ show (fromJust $ single lastInstr)

showEspWithLiveness :: Esp.Metadata a -> [Esp.Method Liveness] -> String
showEspWithLiveness meta mthds = PrintEsp.printTreeWithInstrComments (Esp.Program undefined (undefined <$ meta) mthds)

espressoFile :: FilePath -> FilePath -> FilePath
espressoFile dir file = dir </> file <.> "esp"

espressoCfgFile :: FilePath -> FilePath -> FilePath
espressoCfgFile dir file = dir </> file <.> "cfg"

espressoOptFile :: FilePath -> FilePath -> FilePath
espressoOptFile dir file = dir </> file <.> "opt" <.> "esp"

espressoCfgWithLivenessFile :: FilePath -> FilePath -> FilePath
espressoCfgWithLivenessFile dir file = dir </> file <.> "liv" <.> "cfg"

espressoOptWithLivenessFile :: FilePath -> FilePath -> FilePath
espressoOptWithLivenessFile dir file = dir </> file <.> "liv" <.> "opt" <.> "esp"