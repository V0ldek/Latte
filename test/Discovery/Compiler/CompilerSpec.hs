module Compiler.CompilerSpec (spec) where

import           Compiler
import           Compiler.SSA
import           Control.Monad
import           Data.List
import           Data.Maybe
import           ErrM                   (toEither)
import           Espresso.Interpreter
import           Espresso.Syntax.Abs
import qualified Espresso.Syntax.Parser as ParEspresso (myLexer, pProgram)
import           LatteIO
import           System.Directory       (listDirectory)
import           System.Exit
import           System.FilePath        (replaceExtension, takeBaseName,
                                         takeExtension, (<.>), (</>))
import           Test.Hspec

data LatTest = LatTest {
    tstName     :: String,
    tstContents :: String,
    tstIn       :: [String],
    tstOut      :: [String],
    tstErr      :: [String]
}
data LatResult = Res Int [String] deriving (Eq, Show, Ord)

skip :: [String]
skip = ["primes"]

spec :: Spec
spec = parallel $ do
    describe "Core good" $ do
        tests <- runIO $ getLatTestsFromDir coreGoodDir
        mapM_ goodTest tests
    describe "Core bad" $ do
        tests <- runIO $ getLatTestsFromDir coreBadDir
        mapM_ badTest tests
    describe "Extension struct good" $ do
        tests <- runIO $ getLatTestsFromDir structGoodDir
        mapM_ goodTest tests
    describe "Extension arrays good" $ do
        tests <- runIO $ getLatTestsFromDir arraysGoodDir
        mapM_ goodTest tests
    describe "Extension arrays bad" $ do
        tests <- runIO $ getLatTestsFromDir arraysBadDir
        mapM_ badTest tests
    describe "Extension objects1 good" $ do
        tests <- runIO $ getLatTestsFromDir objects1GoodDir
        mapM_ goodTest tests
    describe "Extension objects1 bad" $ do
        tests <- runIO $ getLatTestsFromDir objects1BadDir
        mapM_ badTest tests
    describe "Extension objects2 good" $ do
        tests <- runIO $ getLatTestsFromDir objects2GoodDir
        mapM_ goodTest tests
    describe "Extension var good" $ do
        tests <- runIO $ getLatTestsFromDir varGoodDir
        mapM_ goodTest tests
    describe "Extension var bad" $ do
        tests <- runIO $ getLatTestsFromDir varBadDir
        mapM_ badTest tests
    describe "Extensions combined" $ do
        tests <- runIO $ getLatTestsFromDir combinedDir
        mapM_ goodTest tests

goodTest :: LatTest -> Spec
goodTest latTest =
    if tstName latTest `elem` skip then runIO $ putStrLn $ "skipping " ++ tstName latTest
    else describe (tstName latTest) $ do
    let fs = StaticFS [StaticF (tstName latTest <.> latExt) (tstContents latTest)]
        out = runStaticIO (go latTest) [] fs
    it (tstName latTest ++ " returns 0") $ LatteIO.staticCode out `shouldBe` ExitSuccess
    it (tstName latTest ++ " is OK") $ LatteIO.staticErr out `shouldBe` tstErr latTest
    testEspresso latTest "Espresso" (espressoFile "." (tstName latTest)) out False
    testEspresso latTest "reachable Espresso" (reachableEspressoFile "." (tstName latTest)) out False
    testEspresso latTest "SSA Espresso" (ssaEspressoFile "." (tstName latTest)) out True
    testEspresso latTest "Optimised Espresso" (optimisedEspressoFile "." (tstName latTest)) out True
    testEspresso latTest "no-phi Espresso" (unfoldedPhiEspressoFile "." (tstName latTest)) out False
    testEspresso latTest "final Espresso" (finalEspressoFile "." (tstName latTest)) out False

testEspresso :: LatTest -> String -> FilePath -> StaticOutput () -> Bool -> Spec
testEspresso latTest descr fp out checkSSA = do
    let esp = staticContents $ fromJust $
            find (\f -> staticPath f == fp <.> "esp") (staticFiles $ staticFS out)
        espPar = toEither $ ParEspresso.pProgram $ ParEspresso.myLexer esp
    case espPar of
        Right espProg -> do
            let espOut = runStaticIO (interpret $ unwrapPos espProg) (tstIn latTest) (StaticFS [])
            it (tstName latTest ++ " " ++ descr ++ " returns 0") $
                LatteIO.staticCode espOut `shouldBe` ExitSuccess
            it (tstName latTest ++ " " ++ descr ++ " output is correct") $
                normaliseOut (LatteIO.staticOut espOut) `shouldBe` normaliseOut (tstOut latTest)
            when checkSSA $ it (tstName latTest ++ " " ++ descr ++ " is SSA") $
                findMultipleAssignments espProg `shouldBe` []
        Left s -> Prelude.error (tstName latTest ++ ": " ++ s)

badTest :: LatTest -> Spec
badTest latTest = do
    let fs = StaticFS [StaticF (tstName latTest <.> latExt) (tstContents latTest)]
        out = runStaticIO (go latTest) [] fs
    it (tstName latTest ++ " gives correct error") $
        normaliseOut (LatteIO.staticErr out) `shouldBe` normaliseOut (tstErr latTest)
    it (tstName latTest ++ " returns 1") $ LatteIO.staticCode out `shouldBe` ExitFailure 1

go :: LatTest -> LatteIO.StaticIO ()
go tst = do
    let options = Opt {verbosity = Quiet, inputFile = tstName tst <.> latExt, generateIntermediate = True}
    run options

getLatTestsFromDir :: FilePath -> IO [LatTest]
getLatTestsFromDir dir = do
    entries <- listDirectory dir
    let latEntries = filter (\f -> takeExtension f == latExt) entries
    mapM latToTest latEntries
    where latToTest f = do
            let outPath = dir </> f `replaceExtension` outExt
                inPath = dir </> f `replaceExtension` inExt
                errPath = dir </> f `replaceExtension` errExt
            hasInput <- doesFileExist inPath
            hasOutput <- doesFileExist outPath
            c <- Prelude.readFile (dir </> f)
            o <- if hasOutput then Prelude.readFile outPath else return []
            i <- if hasInput then Prelude.readFile inPath else return []
            e <- Prelude.readFile errPath
            return $ LatTest (takeBaseName f) c (lines i) (lines o) (lines e)

normaliseOut :: [String] -> String
normaliseOut = unwords . words . unlines

latExt :: String
latExt = ".lat"

errExt :: String
errExt = ".err"

inExt :: String
inExt = ".input"

outExt :: String
outExt = ".output"

coreBadDir :: FilePath
coreBadDir = testRootDir </> "bad"

coreGoodDir :: FilePath
coreGoodDir = testRootDir </> "good"

arraysGoodDir :: FilePath
arraysGoodDir = testRootDir </> "extensions" </> "arrays1" </> "good"

arraysBadDir :: FilePath
arraysBadDir = testRootDir </> "extensions"</> "arrays1" </> "bad"

objects1GoodDir :: FilePath
objects1GoodDir = testRootDir </> "extensions" </> "objects1" </> "good"

objects1BadDir :: FilePath
objects1BadDir = testRootDir </> "extensions" </> "objects1" </> "bad"

objects2GoodDir :: FilePath
objects2GoodDir = testRootDir </> "extensions" </> "objects2"

structGoodDir :: FilePath
structGoodDir = testRootDir </> "extensions" </> "struct"

varGoodDir :: FilePath
varGoodDir = testRootDir </> "extensions" </> "var" </> "good"

varBadDir :: FilePath
varBadDir = testRootDir </> "extensions" </> "var" </> "bad"

combinedDir :: FilePath
combinedDir = testRootDir </> "extensions" </> "combined"

testRootDir :: FilePath
testRootDir = "." </> "test" </> "lattests"
