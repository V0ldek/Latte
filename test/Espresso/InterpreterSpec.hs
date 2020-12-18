module Espresso.InterpreterSpec (spec) where

import           ErrM                   (toEither)
import           Espresso.Interpreter   (interpret)
import           Espresso.Syntax.Abs    (unwrapPos)
import           Espresso.Syntax.Parser (myLexer, pProgram)
import           LatteIO                (StaticIO (runStaticIO))
import           System.Directory       (listDirectory)
import           System.FilePath        (replaceExtension, takeBaseName,
                                         takeExtension, (</>))
import           Test.Hspec

data EspTest = EspTest { name :: String, contents :: String, in_ :: [String], out :: [String], code :: Int }
data EspResult = Res Int [String] deriving (Eq, Show)

spec :: Spec
spec = parallel $ do
    describe "Espresso good" $ do
        tests <- runIO $ getEspTestsFromDir testRootDir
        mapM_ test tests

test :: EspTest -> Spec
test espTest = do
    it (name espTest) $ run (contents espTest) (in_ espTest) `shouldBe` Res (code espTest) (out espTest)

run :: String -> [String] -> EspResult
run s input = case toEither $ pProgram ts of
    Left e  -> Prelude.error e
    Right t -> case runStaticIO (interpret $ unwrapPos t) input of
                 Left e -> Prelude.error e
                 Right (exitCode, output, remInput) -> if not $ null remInput
                     then Prelude.error "Not all input consumed"
                     else Res exitCode output
    where ts = myLexer s

getEspTestsFromDir :: FilePath -> IO [EspTest]
getEspTestsFromDir dir = do
    entries <- listDirectory dir
    let espEntries = filter (\f -> takeExtension f == espExt) entries
    mapM espToTest espEntries
    where espToTest f = do
            c <- readFile (dir </> f)
            i <- readFile (dir </> f `replaceExtension` inExt)
            o <- readFile (dir </> f `replaceExtension` outExt)
            return $ EspTest (takeBaseName f) c (lines i) (lines o) 0

espExt :: String
espExt = ".esp"

inExt :: String
inExt = ".in"

outExt :: String
outExt = ".out"

testRootDir :: FilePath
testRootDir = "." </> "test" </> "esptests"
