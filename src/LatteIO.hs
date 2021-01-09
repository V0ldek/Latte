module LatteIO where

import           Data.List
import           Data.Maybe
import           Prelude          hiding (error)
import qualified System.Directory as Directory (doesDirectoryExist,
                                                doesFileExist)
import qualified System.Exit      as Exit (ExitCode (..), exitFailure,
                                           exitSuccess, exitWith)
import           System.IO        (hPutStrLn, stderr)

class LatteIO m where
    readInt :: m Int
    readString :: m String
    error :: String -> m a
    printInt :: Int -> m ()
    printString :: String -> m ()
    printErrorString :: String -> m ()
    doesDirectoryExist :: FilePath -> m Bool
    doesFileExist :: FilePath -> m Bool
    readFile :: FilePath -> m String
    writeFile :: FilePath -> String -> m ()
    exitWith :: Exit.ExitCode -> m a
    exitSuccess :: m a
    exitFailure :: m a

    printInt = printString . show
    exitSuccess = exitWith Exit.ExitSuccess
    exitFailure = exitWith $ Exit.ExitFailure 1

instance LatteIO IO where
    readInt = read <$> readString
    readString = getLine
    error = fail . ("runtime error\n" ++)
    printString = putStrLn
    printErrorString = hPutStrLn stderr
    doesDirectoryExist = Directory.doesDirectoryExist
    doesFileExist = Directory.doesFileExist
    readFile = Prelude.readFile
    writeFile = Prelude.writeFile
    exitWith = Exit.exitWith
    exitSuccess = Exit.exitSuccess
    exitFailure = Exit.exitFailure

newtype StaticFileSystem = StaticFS {
    staticFiles :: [StaticFile]
} deriving Show

data StaticFile = StaticF {
    staticPath     :: FilePath,
    staticContents :: String
} deriving Show

data StaticOutput a = StaticO {
    staticCode  :: Exit.ExitCode,
    staticVal   :: Maybe a,
    staticOut   :: [String],
    staticErr   :: [String],
    staticRemIn :: [String],
    staticFS    :: StaticFileSystem
}

emptyFileSystem :: StaticFileSystem
emptyFileSystem = StaticFS []

emptyOutput :: a -> [String] -> StaticFileSystem -> StaticOutput a
emptyOutput x = StaticO Exit.ExitSuccess (Just x) [] []

errorOutput :: String -> [String] -> StaticFileSystem -> StaticOutput a
errorOutput s = StaticO (Exit.ExitFailure 1) Nothing ["runtime error", s] []

newtype StaticIO a = StaticIO {
    runStaticIO :: [String] -> StaticFileSystem -> StaticOutput a
}

instance Functor StaticIO where
    fmap f s = StaticIO (\in_ fs -> let x = runStaticIO s in_ fs in x {staticVal = f <$> staticVal x})

instance Applicative StaticIO where
    pure x = StaticIO (emptyOutput x)
    f <*> g = StaticIO (\in_ fs ->
         let xf = runStaticIO f in_ fs
             xg = runStaticIO g (staticRemIn xf) (staticFS xf)
         in  xg {
            staticCode = if staticCode xf == Exit.ExitSuccess then staticCode xg else staticCode xf,
            staticVal = staticVal xf <*> staticVal xg,
            staticOut = staticOut xf ++ staticOut xg,
            staticErr = staticErr xf ++ staticErr xg
         })

instance Monad StaticIO where
    return = pure
    f >>= g = StaticIO (\in_ fs ->
        let xf = runStaticIO f in_ fs
        in  if staticCode xf /= Exit.ExitSuccess || isNothing (staticVal xf) then xf {staticVal = Nothing}
            else
        let g' = g (fromJust $ staticVal xf)
            xg = runStaticIO g' (staticRemIn xf) (staticFS xf)
        in  xg {
                staticOut = staticOut xf ++ staticOut xg,
                staticErr = staticErr xf ++ staticErr xg
            })

instance MonadFail StaticIO where
    fail s = StaticIO (\in_ fs -> (errorOutput s in_ fs) {staticOut = [], staticErr = [s]})

staticGetLine :: StaticIO String
staticGetLine = StaticIO (\in_ fs -> case in_ of
        []         -> errorOutput "unexpected eof" in_ fs
        (x : in_') -> emptyOutput x in_' fs)

staticError :: String -> StaticIO a
staticError s = StaticIO (errorOutput s)

staticPutStrLn :: String -> StaticIO ()
staticPutStrLn out = StaticIO (\in_ fs -> (emptyOutput () in_ fs) {staticOut = [out]})

staticPutStrErrLn :: String -> StaticIO ()
staticPutStrErrLn err = StaticIO (\in_ fs -> (emptyOutput () in_ fs) {staticErr = [err]})

staticExitWith :: Exit.ExitCode -> StaticIO a
staticExitWith ec = StaticIO (StaticO ec Nothing [] [])

staticDoesDirectoryExist :: FilePath -> StaticIO Bool
staticDoesDirectoryExist _ = return True

staticDoesFileExist :: FilePath -> StaticIO Bool
staticDoesFileExist fp = StaticIO (\in_ fs ->
    let exists = any (\f -> staticPath f == fp) (staticFiles fs)
    in  emptyOutput exists in_ fs)

staticReadFile :: FilePath -> StaticIO String
staticReadFile fp = do
    exists <- staticDoesFileExist fp
    if not exists then fail $ "file not found " ++ show fp else StaticIO (\in_ fs ->
        let file = fromJust $ find (\f -> staticPath f == fp) (staticFiles fs)
        in  emptyOutput (staticContents file) in_ fs)

staticWriteFile :: FilePath -> String -> StaticIO ()
staticWriteFile fp contents = StaticIO (\in_ fs ->
    let files = staticFiles fs
        newFile = StaticF fp contents
        newFiles = newFile:filter (\f -> staticPath f /= fp) files
    in  emptyOutput () in_ (fs {staticFiles = newFiles}))

instance LatteIO StaticIO where
    readInt = read <$> readString
    readString = staticGetLine
    error = staticError
    printString = staticPutStrLn
    printErrorString = staticPutStrErrLn
    doesDirectoryExist = staticDoesDirectoryExist
    doesFileExist = staticDoesFileExist
    readFile = staticReadFile
    writeFile = staticWriteFile
    exitWith = staticExitWith
