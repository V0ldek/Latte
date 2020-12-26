module Espresso.GeneratorSpec (spec) where

import           ErrM                       (toEither)
import           Espresso.CodeGen.Generator (generateEspresso)
import           Espresso.Interpreter       (interpret)
import qualified Espresso.Syntax.Abs        as AbsEspresso
import qualified Espresso.Syntax.Parser     as ParseEspresso
import qualified Espresso.Syntax.Printer    as PrintEspresso
import           LatteIO                    (StaticIO (runStaticIO))
import           SemanticAnalysis.Analyser  (analyse)
import           SemanticAnalysis.TopLevel  (programMetadata)
import           Syntax.Abs                 (unwrapPos)
import           Syntax.Parser              (myLexer, pProgram)
import           Syntax.Rewriter            (rewrite)
import           System.Directory           (doesFileExist, listDirectory)
import           System.FilePath            (replaceExtension, takeBaseName,
                                             takeExtension, (</>))
import           Test.Hspec

data LatTest = LatTest { name :: String, contents :: String, out :: String, in_ :: Maybe String }
data LatResult = Ok String | Error String deriving (Eq, Show, Ord)

spec :: Spec
spec = parallel $ do
    describe "Core good" $ do
        tests <- runIO $ getLatTestsFromDir coreGoodDir
        mapM_ goodTest tests
    {-describe "Extension struct good" $ do
        tests <- runIO $ getLatTestsFromDir structGoodDir
        mapM_ goodTest tests
    describe "Extension arrays good" $ do
        tests <- runIO $ getLatTestsFromDir arraysGoodDir
        mapM_ goodTest tests
    describe "Extension objects1 good" $ do
        tests <- runIO $ getLatTestsFromDir objects1GoodDir
        mapM_ goodTest tests
    describe "Extension objects2 good" $ do
        tests <- runIO $ getLatTestsFromDir objects2GoodDir
        mapM_ goodTest tests
    describe "Extension var good" $ do
        tests <- runIO $ getLatTestsFromDir varGoodDir
        mapM_ goodTest tests-}

goodTest :: LatTest -> Spec
goodTest latTest = do
    it (name latTest) $ run (contents latTest) (in_ latTest) `shouldBe` Ok (out latTest)

run :: String -> Maybe String -> LatResult
run s input = case toEither $ pProgram ts of
    Left e  -> Error $ "ERROR\n" ++ e
    Right t -> case programMetadata (rewrite $ unwrapPos t) of
        Left e  -> Error $ "ERROR\n" ++ e
        Right m -> case analyse m of
            Left e  -> Error $ "ERROR\n" ++ e
            Right m' -> let esp = generateEspresso m'
                            esp' = roundTripEspresso esp
                            input' = maybe [] lines input
                        in case runStaticIO (interpret esp') input' of
                            Right (_, output, _) -> Ok (unlines output)
                            Left e               -> Error e
    where ts = myLexer s
          roundTripEspresso esp =
              case toEither $ ParseEspresso.pProgram $ ParseEspresso.myLexer $ PrintEspresso.printTree esp of
                  Right t -> AbsEspresso.unwrapPos t
                  Left e  -> error e

getLatTestsFromDir :: FilePath -> IO [LatTest]
getLatTestsFromDir dir = do
    entries <- listDirectory dir
    let latEntries = filter (\f -> takeExtension f == latExt) entries
    mapM latToTest latEntries
    where latToTest f = do
            let outPath = dir </> f `replaceExtension` outExt
                inPath = dir </> f `replaceExtension` inExt
            c <- readFile (dir </> f)
            e <- readFile outPath
            hasInput <- doesFileExist inPath
            i <- if hasInput then Just <$> readFile inPath else return Nothing
            return $ LatTest (takeBaseName f) c e i

latExt :: String
latExt = ".lat"

outExt :: String
outExt = ".output"

inExt :: String
inExt = ".input"

coreGoodDir :: FilePath
coreGoodDir = testRootDir </> "good"

{-
arraysGoodDir :: FilePath
arraysGoodDir = testRootDir </> "extensions" </> "arrays1"

objects1GoodDir :: FilePath
objects1GoodDir = testRootDir </> "extensions" </> "objects1" </> "good"

objects2GoodDir :: FilePath
objects2GoodDir = testRootDir </> "extensions" </> "objects2"

structGoodDir :: FilePath
structGoodDir = testRootDir </> "extensions" </> "struct"

varGoodDir :: FilePath
varGoodDir = testRootDir </> "extensions" </> "var" </> "good"
-}

testRootDir :: FilePath
testRootDir = "." </> "test" </> "lattests"
