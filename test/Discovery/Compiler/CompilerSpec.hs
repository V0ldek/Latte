module Compiler.CompilerSpec (spec) where

import           Compiler
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
    dont $ describe "Extension objects1 good" $ do
        tests <- runIO $ getLatTestsFromDir objects1GoodDir
        mapM_ goodTest tests
    describe "Extension objects1 bad" $ do
        tests <- runIO $ getLatTestsFromDir objects1BadDir
        mapM_ badTest tests
    dont $ describe "Extension objects2 good" $ do
        tests <- runIO $ getLatTestsFromDir objects2GoodDir
        mapM_ goodTest tests
    dont $ describe "Extension var good" $ do
        tests <- runIO $ getLatTestsFromDir varGoodDir
        mapM_ goodTest tests
    describe "Extension var bad" $ do
        tests <- runIO $ getLatTestsFromDir varBadDir
        mapM_ badTest tests

dont :: SpecWith a -> SpecWith ()
dont _ = return ()

goodTest :: LatTest -> Spec
goodTest latTest =
    if tstName latTest `elem` skip then runIO $ putStrLn $ "skipping " ++ tstName latTest
    else describe (tstName latTest) $ do
    let fs = StaticFS [StaticF (tstName latTest <.> latExt) (tstContents latTest)]
        out = runStaticIO (go latTest) [] fs
        esp = staticContents $ fromJust $
            find (\f -> staticPath f == espressoFile "." (tstName latTest)) (staticFiles $ staticFS out)
        espLin = staticContents $ fromJust $
            find (\f -> staticPath f == espressoWithoutDeadFile "." (tstName latTest)) (staticFiles $ staticFS out)
        espPhi = staticContents $ fromJust $
            find (\f -> staticPath f == espressoWithUnfoldedPhiFile "." (tstName latTest)) (staticFiles $ staticFS out)
        espOpt = staticContents $ fromJust $
            find (\f -> staticPath f == espressoOptimisedFile "." (tstName latTest)) (staticFiles $ staticFS out)
        asm = staticContents $ fromJust $
            find (\f -> staticPath f == assemblyFile "." (tstName latTest)) (staticFiles $ staticFS out)
        espPar = toEither $ ParEspresso.pProgram $ ParEspresso.myLexer esp
        espLinPar = toEither $ ParEspresso.pProgram $ ParEspresso.myLexer espLin
        espPhiPar = toEither $ ParEspresso.pProgram $ ParEspresso.myLexer espPhi
        espOptPar = toEither $ ParEspresso.pProgram $ ParEspresso.myLexer espOpt
    it (tstName latTest ++ " returns 0") $ LatteIO.staticCode out `shouldBe` ExitSuccess
    case sequence [espPar, espLinPar, espPhiPar, espOptPar] of
        Right [espProg, espLinProg, espPhiProg, espOptProg] -> do
            let espOut = runStaticIO (interpret $ unwrapPos espProg) (tstIn latTest) (StaticFS [])
                espLinOut = runStaticIO (interpret $ unwrapPos espLinProg) (tstIn latTest) (StaticFS [])
                espPhiOut = runStaticIO (interpret $ unwrapPos espPhiProg) (tstIn latTest) (StaticFS [])
                espOptOut = runStaticIO (interpret $ unwrapPos espOptProg) (tstIn latTest) (StaticFS [])
            it (tstName latTest ++ " is OK") $ LatteIO.staticErr out `shouldBe` tstErr latTest
            it (tstName latTest ++ " Espresso returns 0") $
                LatteIO.staticCode espOut `shouldBe` ExitSuccess
            it (tstName latTest ++ " Espresso output is correct") $
                normaliseOut (LatteIO.staticOut espOut) `shouldBe` normaliseOut (tstOut latTest)
            it (tstName latTest ++ " linearised Espresso returns 0") $
                LatteIO.staticCode espLinOut `shouldBe` ExitSuccess
            it (tstName latTest ++ " linearised Espresso output is correct") $
                normaliseOut (LatteIO.staticOut espLinOut) `shouldBe` normaliseOut (tstOut latTest)
            it (tstName latTest ++ " unfolded phis Espresso returns 0") $
                LatteIO.staticCode espPhiOut `shouldBe` ExitSuccess
            it (tstName latTest ++ " unfolded phis Espresso output is correct") $
                normaliseOut (LatteIO.staticOut espPhiOut) `shouldBe` normaliseOut (tstOut latTest)
            it (tstName latTest ++ " unfolded phis Espresso returns 0") $
                LatteIO.staticCode espOptOut `shouldBe` ExitSuccess
            it (tstName latTest ++ " unfolded phis Espresso output is correct") $
                normaliseOut (LatteIO.staticOut espOptOut) `shouldBe` normaliseOut (tstOut latTest)
            it (tstName latTest ++ " x86_64 output is nonempty") $
                null asm `shouldBe` False
        Right _ -> Prelude.error "impossible"
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

testRootDir :: FilePath
testRootDir = "." </> "test" </> "lattests"
