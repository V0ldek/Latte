module Espresso.InterpreterSpec (spec) where

import           ErrM                   (toEither)
import           Espresso.Interpreter   (interpret)
import           Espresso.Syntax.Abs    (unwrapPos)
import           Espresso.Syntax.Parser (myLexer, pProgram)
import           LatteIO
import           System.Directory       (listDirectory)
import           System.Exit
import           System.FilePath        (replaceExtension, takeBaseName,
                                         takeExtension, (</>))
import           Test.Hspec

data EspTest = EspTest { tstName :: String, tstContents :: String, tstIn :: [String], tstOut :: [String] }

spec :: Spec
spec = parallel $ do
    describe "Espresso good" $ do
        tests <- runIO $ getEspTestsFromDir testRootDir
        mapM_ test tests

test :: EspTest -> Spec
test espTest = do
    let out = run (tstContents espTest) (tstIn espTest)
    it (tstName espTest ++ " returns 0") $ staticCode out `shouldBe` ExitSuccess
    it (tstName espTest ++ " consumes all input") $ staticRemIn out `shouldBe` []
    it (tstName espTest ++ " gives correct output") $ unlines (staticOut out) `shouldBe` unlines (tstOut espTest)

run :: String -> [String] -> StaticOutput ()
run s input = case toEither $ pProgram $ myLexer s of
    Left e  -> Prelude.error e
    Right t -> runStaticIO (interpret $ unwrapPos t) input (StaticFS [])

getEspTestsFromDir :: FilePath -> IO [EspTest]
getEspTestsFromDir dir = do
    entries <- listDirectory dir
    let espEntries = filter (\f -> takeExtension f == espExt) entries
    mapM espToTest espEntries
    where espToTest f = do
            c <- Prelude.readFile (dir </> f)
            i <- Prelude.readFile (dir </> f `replaceExtension` inExt)
            o <- Prelude.readFile (dir </> f `replaceExtension` outExt)
            return $ EspTest (takeBaseName f) c (lines i) (lines o)

espExt :: String
espExt = ".esp"

inExt :: String
inExt = ".in"

outExt :: String
outExt = ".out"

testRootDir :: FilePath
testRootDir = "." </> "test" </> "esptests"
