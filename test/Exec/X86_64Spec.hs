module X86_64Spec where

import           Control.Monad
import           Data.Char
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.Hspec
import           Utilities

data ExecTestSet = TestSet {setName :: String, setTests :: [ExecTest]}
data ExecTest = Test {testName :: String, testPath :: FilePath}

-- If True, the workdir is removed after execution ends.
-- Set to False to inspect the compiler output when tests fail.
cleanupEnabled :: Bool
cleanupEnabled = False

main :: IO ()
main = do
    sets <- testSets
    prepareTests $ concatMap (map testPath . setTests) sets
    hspec $ runTests sets

runTests :: [ExecTestSet] -> Spec
runTests sets = do
    parallel $ forM_ sets (\s -> describe (setName s) $ forM_ (map testName (setTests s)) runTest)
    runIO cleanup

runTest :: FilePath -> Spec
runTest test = do
    let latFile = workDirectory </> test <.> latExtension
        inputFile = workDirectory </> test <.> inputExtension
        outputFile = workDirectory </> test <.> expectedOutputExtension
        actualOutputFile = workDirectory </> test <.> actualOutputExtension
        execFile = workDirectory </> test
    latcExitCode <- runIO $ runPrtCommand (compilerPath ++ " -g " ++ latFile) >>= waitForProcess
    execExists <- runIO $ doesFileExist execFile
    execExitCode <- runIO $ runPrtCommand (execFile ++ " < " ++ inputFile ++ " > " ++ actualOutputFile)
        >>= waitForProcess
    expectedOutputExists <- runIO $ doesFileExist outputFile
    actualOutputExists <- runIO $ doesFileExist actualOutputFile
    expectedOutput <- runIO $ if expectedOutputExists then readFile outputFile else return ""
    actualOutput <- runIO $ if actualOutputExists then readFile actualOutputFile else return ""
    it (test ++ " compiles successfully") $ latcExitCode `shouldBe` ExitSuccess
    it (test ++ " executable is created") $ execExists `shouldBe` True
    it (test ++ " executes successfully") $ execExitCode `shouldBe` ExitSuccess
    it (test ++ " gives correct output") $ normaliseOut actualOutput `shouldBe` normaliseOut expectedOutput

prepareTests :: [FilePath] -> IO ()
prepareTests tests = do
    makeExitCode <- runPrtCommand "make" >>= waitForProcess
    unless (makeExitCode == ExitSuccess) (putStr "make failed with exit code " >> print makeExitCode >> exitFailure)
    mkdirExitCode <- runPrtCommand ("mkdir " ++ workDirectory) >>= waitForProcess
    unless (mkdirExitCode == ExitSuccess) (putStr "mkdir failed with exit code " >> print mkdirExitCode >> exitFailure)
    forM_ tests copyTestToWorkdir
    where copyTestToWorkdir fp = do
                let latFile = fp <.> latExtension
                    inputFile = fp <.> inputExtension
                    outputFile = fp <.> expectedOutputExtension
                cpExitCode <-
                    runPrtCommand ("cp " ++ latFile ++ " " ++ outputFile ++ " " ++ workDirectory)
                    >>= waitForProcess
                unless (cpExitCode == ExitSuccess) (failCmd "cp" cpExitCode)
                inputExists <- doesFileExist inputFile
                if inputExists
                    then do
                        cpExitCode2 <- runPrtCommand ("cp " ++ inputFile ++ " " ++ workDirectory)
                            >>= waitForProcess
                        unless (cpExitCode2 == ExitSuccess) (failCmd "cp" cpExitCode2)
                    else do
                        let targetOutput = workDirectory </> takeFileName fp <.> inputExtension
                        touchExitCode <- runPrtCommand ("touch " ++ targetOutput) >>= waitForProcess
                        unless (touchExitCode == ExitSuccess) (failCmd "touch" touchExitCode)

runPrtCommand :: String -> IO ProcessHandle
runPrtCommand s = putStrLn ("running: " ++ s) >> runCommand s

cleanup :: IO ()
cleanup = when cleanupEnabled (do
    rmExitCode <- runPrtCommand ("rm -rf " ++ workDirectory) >>= waitForProcess
    unless (rmExitCode == ExitSuccess) (failCmd "rm" rmExitCode))

normaliseOut :: String -> String
normaliseOut s = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse s

testSets :: IO [ExecTestSet]
testSets = sequence [coreTestSet, structTestSet, arrayTestSet, objectsTestSet, callvirtTestSet, varTestSet, combinedTestSet]

coreTestSet :: IO ExecTestSet
coreTestSet = getTestSet coreTestsDirectory "Core"

arrayTestSet :: IO ExecTestSet
arrayTestSet = getTestSet arrayTestDirectory "Array"

structTestSet :: IO ExecTestSet
structTestSet = getTestSet structTestDirectory "Struct"

objectsTestSet :: IO ExecTestSet
objectsTestSet = getTestSet objects1TestDirectory "Objects"

callvirtTestSet :: IO ExecTestSet
callvirtTestSet = getTestSet objects2TestDirectory "Callvirt"

combinedTestSet :: IO ExecTestSet
combinedTestSet = getTestSet combinedTestDirectory "Combi9ed"

varTestSet :: IO ExecTestSet
varTestSet = getTestSet varTestDictionary "Var"

getTestSet :: FilePath -> String -> IO ExecTestSet
getTestSet dir name = do
    tests <- getTestsFromDir dir
    return $ TestSet name tests

getTestsFromDir :: FilePath -> IO [ExecTest]
getTestsFromDir dir =  do
    files <- listDirectory dir
    let lats = filter (\f -> takeExtension f == latExtension) files
        tests = map dropExtension lats
    return $ map (\t -> Test (takeFileName t) (dir </> t)) (dedup tests)

compilerPath :: FilePath
compilerPath = "." </> "latc_x86_64"

testDirectoryRoot :: FilePath
testDirectoryRoot = "." </> "test" </> "lattests"

coreTestsDirectory :: FilePath
coreTestsDirectory = testDirectoryRoot </> "good"

structTestDirectory :: FilePath
structTestDirectory = testDirectoryRoot </> "extensions" </> "struct"

arrayTestDirectory :: FilePath
arrayTestDirectory = testDirectoryRoot </> "extensions" </> "arrays1" </> "good"

objects1TestDirectory :: FilePath
objects1TestDirectory = testDirectoryRoot </> "extensions" </> "objects1" </> "good"

objects2TestDirectory :: FilePath
objects2TestDirectory = testDirectoryRoot </> "extensions" </> "objects2"

combinedTestDirectory :: FilePath
combinedTestDirectory = testDirectoryRoot </> "extensions" </> "combined"

varTestDictionary :: FilePath
varTestDictionary = testDirectoryRoot </> "extensions" </> "var" </> "good"

workDirectory :: FilePath
workDirectory = "." </> "test" </> "x86_64Spec_workdir"

latExtension :: FilePath
latExtension = ".lat"

inputExtension :: FilePath
inputExtension = ".input"

expectedOutputExtension :: FilePath
expectedOutputExtension = ".output"

actualOutputExtension :: FilePath
actualOutputExtension = ".output.actual"

failCmd :: String -> ExitCode -> IO ()
failCmd s c = putStr (s ++ " failed with exit code ") >> print c >> cleanup >> exitFailure
