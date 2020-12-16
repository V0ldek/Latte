module SemanticAnalysis.AcceptanceSpec (spec) where

import           ErrM                      (toEither)
import           SemanticAnalysis.Analyser (analyse)
import           SemanticAnalysis.Toplevel (programMetadata)
import           Syntax.Abs                (unwrapPos)
import           Syntax.Parser             (myLexer, pProgram)
import           Syntax.Rewriter           (rewrite)
import           System.Directory          (listDirectory)
import           System.FilePath           (replaceExtension, takeBaseName,
                                            takeExtension, (</>))
import           Test.Hspec

data LatTest = LatTest { name :: String, contents :: String, err :: String }
data LatResult = Ok | Error String deriving (Eq, Show, Ord)

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

goodTest :: LatTest -> Spec
goodTest latTest = do
    it (name latTest) $ run (contents latTest) `shouldBe` Ok

badTest :: LatTest -> Spec
badTest latTest = do
    it (name latTest) $ run (contents latTest) `shouldBe` Error (err latTest)

run :: String -> LatResult
run s = case toEither $ pProgram ts of
    Left e  -> Error $ "ERROR\n" ++ e
    Right t -> case programMetadata (rewrite $ unwrapPos t) of
        Left e  -> Error $ "ERROR\n" ++ e
        Right m -> case analyse m of
            Left e  -> Error $ "ERROR\n" ++ e
            Right _ -> Ok
    where ts = myLexer s

getLatTestsFromDir :: FilePath -> IO [LatTest]
getLatTestsFromDir dir = do
    entries <- listDirectory dir
    let latEntries = filter (\f -> takeExtension f == latExt) entries
    mapM latToTest latEntries
    where latToTest f = do
            c <- readFile (dir </> f)
            e <- readFile (dir </> f `replaceExtension` errExt)
            return $ LatTest (takeBaseName f) c e

latExt :: String
latExt = ".lat"

errExt :: String
errExt = ".err"

coreBadDir :: FilePath
coreBadDir = testRootDir </> "bad"

coreGoodDir :: FilePath
coreGoodDir = testRootDir </> "good"

arraysGoodDir :: FilePath
arraysGoodDir = testRootDir </> "extensions" </> "arrays1"

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
