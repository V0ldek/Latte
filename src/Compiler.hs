module Compiler where

import           Control.Monad                      (when)
import           Data.Bifunctor                     (Bifunctor (first))
import qualified Data.Map                           as Map
import           ErrM                               (toEither)
import           Espresso.CodeGen.Generator         (generateEspresso)
import           Espresso.ControlFlow.CFG
import           Espresso.ControlFlow.Liveness      (Liveness, analyseLiveness,
                                                     emptyLiveness)
import           Espresso.ControlFlow.Phi           (unfoldPhi)
import           Espresso.ControlFlow.SSA
import           Espresso.Optimisation.CFGTransform (inlineTrivialBlocks,
                                                     removeUnreachable)
import           Espresso.Optimisation.Pipeline
import qualified Espresso.Syntax.Abs                as Esp
import           Espresso.Syntax.Printer            as PrintEsp (Print,
                                                                 printTree,
                                                                 printTreeWithInstrComments)
import           Identifiers                        (ToString (toStr))
import           LatteIO
import           SemanticAnalysis.Analyser          (SemData, analyse)
import           SemanticAnalysis.TopLevel          (Metadata, programMetadata)
import           Syntax.Abs                         as Latte (Pos, Program,
                                                              unwrapPos)
import           Syntax.Lexer                       (Token)
import           Syntax.Parser                      (myLexer, pProgram)
import           Syntax.Printer                     as PrintLatte (Print,
                                                                   printTree)
import           Syntax.Rewriter                    (rewrite)
import           System.FilePath                    (dropExtension,
                                                     takeDirectory,
                                                     takeFileName, (<.>), (</>))
import           Utilities                          (unlessM)
import           X86_64.CodeGen.Generator           (generate)
import qualified X86_64.Optimisation.Peephole       as Peephole

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

    let espresso@(Esp.Program _ meta mthds) = generateEspresso latte
        cfgs = zip (map cfg mthds) mthds
        cfgsLin = map (uncurry removeUnreachable) cfgs
        cfgsWithLiveness = map (first analyseLiveness) cfgsLin
        ssaCode = map (\(g, mthd) -> (transformToSSA g mthd, mthd)) cfgsWithLiveness
        optimisedCode = map (\(ssa, mthd) -> (optimise ssa mthd, mthd)) ssaCode
        unfoldedPhiCode = zip (map (unfoldPhi . first unwrapSSA) optimisedCode) mthds
        optimisedCfgs = map (\(cfg_, mthd) -> removeUnreachable (inlineTrivialBlocks cfg_) mthd) unfoldedPhiCode
        optimisedWithLiveness = map (first analyseLiveness) optimisedCfgs
    printStringV v "Brewing Espresso..."
    genStep opt (espressoFile directory fileName <.> "esp") (PrintEsp.printTree espresso)
    printStringV v "Building CFGs..."
    genStep opt (espressoFile directory fileName <.> "cfg") (showCfgs cfgs)
    genEspStep opt reachableEspressoFile meta cfgsLin "Removing unreachable blocks..."
    genEspWithLivenessStep opt livenessEspressoFile meta cfgsWithLiveness "Analysing liveness..."
    genEspStep opt ssaEspressoFile meta (map (first unwrapSSA) ssaCode) "Transforming to SSA..."
    genEspStep opt optimisedEspressoFile meta (map (first unwrapSSA) optimisedCode) "Optimising Espresso..."
    genEspStep opt unfoldedPhiEspressoFile meta unfoldedPhiCode "Unfolding phis..."
    genEspStep opt finalEspressoFile meta optimisedCfgs "Inlining trivial jumps..."
    genEspWithLivenessStep opt finalLivenessEspressoFile meta optimisedWithLiveness "Reanalysing liveness..."

    printStringV v "Generating x86_64 assembly..."
    let assembly = generate meta optimisedWithLiveness
    genStep opt (unoptAssemblyFile directory fileName) assembly
    let optAssembly = unlines $ Peephole.optimise (lines assembly)
    genOutput opt (assemblyFile directory fileName) optAssembly

genEspStep :: (Monad m, LatteIO m) => Options -> (FilePath -> FilePath -> FilePath) -> Esp.Metadata () -> [(CFG (), Esp.Method ())] -> String -> m ()
genEspStep opt fp meta cfgs comment = do
    let f = inputFile opt
        v = verbosity opt
        fileName = dropExtension $ takeFileName f
        directory = takeDirectory f
        esp = PrintEsp.printTree $ Esp.Program () meta (cfgsToMthds () cfgs)
    printStringV v comment
    genStep opt (fp directory fileName  <.> "cfg") (showCfgs cfgs)
    genStep opt (fp directory fileName  <.> "esp") esp

genEspWithLivenessStep :: (Monad m, LatteIO m) => Options -> (FilePath -> FilePath -> FilePath) -> Esp.Metadata () -> [(CFG Liveness, Esp.Method ())] -> String -> m ()
genEspWithLivenessStep opt fp meta cfgs comment = do
    let f = inputFile opt
        v = verbosity opt
        fileName = dropExtension $ takeFileName f
        directory = takeDirectory f
        esp = showEspWithLiveness meta (cfgsToMthds emptyLiveness cfgs)
    printStringV v comment
    genStep opt (fp directory fileName <.> "cfg") (showCfgsWithLiveness cfgs)
    genStep opt (fp directory fileName <.> "esp") esp

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

genOutput :: (Monad m, LatteIO m) => Options -> FilePath -> String -> m ()
genOutput opt fp contents = do
    let v = verbosity opt
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
      printStringV v $ "\n[linearised tree]\n\n" ++ PrintLatte.printTree tree

showEspTree :: (Monad m, LatteIO m, Show a, PrintEsp.Print a) => Verbosity -> a -> m ()
showEspTree v tree
 = do
      printStringV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      printStringV v $ "\n[linearised tree]\n\n" ++ PrintEsp.printTree tree

showCfgs :: [(CFG a, Esp.Method a)] -> String
showCfgs cfgs = unlines $ map showCfg cfgs
  where
    showCfg (g, Esp.Mthd _ _ (Esp.QIdent _ (Esp.SymIdent i1) (Esp.SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show g

cfgsToMthds ::  a -> [(CFG a, Esp.Method b)] -> [Esp.Method a]
cfgsToMthds default_ = map (\(g, Esp.Mthd _ r i ps _) ->
    Esp.Mthd default_ (default_ <$ r) (default_ <$ i) (map (default_ <$) ps) (linearise g))

showCfgsWithLiveness :: [(CFG Liveness, Esp.Method a)] -> String
showCfgsWithLiveness cfgs = unlines $ map showCfg cfgs
  where
    showCfg (CFG g, Esp.Mthd _ _ (Esp.QIdent _ (Esp.SymIdent i1) (Esp.SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show (CFG g) ++ concatMap showLiveness (Map.elems g)
    showLiveness node =
        "Liveness at start of " ++ toStr (nodeLabel node) ++ ": " ++ show (nodeHead node) ++ "\n" ++
           "Liveness at end of " ++ toStr (nodeLabel node) ++ ": " ++ show (nodeTail node) ++ "\n"

showEspWithLiveness :: Esp.Metadata a -> [Esp.Method Liveness] -> String
showEspWithLiveness meta mthds = PrintEsp.printTreeWithInstrComments (Esp.Program emptyLiveness (emptyLiveness <$ meta) mthds)

assemblyFile :: FilePath -> FilePath -> FilePath
assemblyFile dir file = dir </> file <.> "s"

unoptAssemblyFile :: FilePath -> FilePath -> FilePath
unoptAssemblyFile dir file = dir </> file <.> "noopt" <.> "s"

espressoFile :: FilePath -> FilePath -> FilePath
espressoFile dir file = dir </> file

reachableEspressoFile :: FilePath -> FilePath -> FilePath
reachableEspressoFile dir file = dir </> file <.> "1" <.> "reach"

livenessEspressoFile :: FilePath -> FilePath -> FilePath
livenessEspressoFile dir file = dir </> file <.> "2" <.> "liv"

ssaEspressoFile :: FilePath -> FilePath -> FilePath
ssaEspressoFile dir file = dir </> file <.> "3" <.> "ssa"

optimisedEspressoFile :: FilePath -> FilePath -> FilePath
optimisedEspressoFile dir file = dir </> file <.> "4" <.> "opt"

unfoldedPhiEspressoFile :: FilePath -> FilePath -> FilePath
unfoldedPhiEspressoFile dir file = dir </> file <.> "5" <.> "nophi"

finalEspressoFile :: FilePath -> FilePath -> FilePath
finalEspressoFile dir file = dir </> file <.> "6" <.> "final"

finalLivenessEspressoFile :: FilePath -> FilePath -> FilePath
finalLivenessEspressoFile dir file = dir </> file <.> "7" <.> "final" <.> "liv"
