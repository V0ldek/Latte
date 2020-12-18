{-# LANGUAGE LambdaCase #-}
module LatteIO where

import           Prelude hiding (error)

class LatteIO m where
    readInt :: m Int
    readString :: m String
    error :: m a
    printInt :: Int -> m ()
    printString :: String -> m ()

    printInt = printString . show

instance LatteIO IO where
    readInt = read <$> readString
    readString = getLine
    error = fail "runtime error"
    printString = putStrLn

newtype StaticIO a = StaticIO { runStaticIO :: [String] -> Either String (a, [String], [String]) }

instance Functor StaticIO where
    fmap f s = StaticIO (\in_ -> do
        (x, out, in_') <- runStaticIO s in_
        return (f x, out, in_'))

instance Applicative StaticIO where
    pure x = StaticIO (\in_ -> return (x, [], in_))
    f <*> g = StaticIO (\in_ -> do
        (xf, outf, inf) <- runStaticIO f in_
        (xg, outg, ing) <- runStaticIO g inf
        return (xf xg, outf ++ outg, ing))

instance Monad StaticIO where
    return = pure
    f >>= g = StaticIO (\in_ -> do
        (xf, outf, inf) <- runStaticIO f in_
        let g' = g xf
        (xg, outg, ing) <- runStaticIO g' inf
        return (xg, outf ++ outg, ing))

instance MonadFail StaticIO where
    fail x = StaticIO (\_ -> Left x)

staticGetLine :: StaticIO String
staticGetLine = StaticIO (\case
        []        -> Left "unexpected eof"
        (x : in_) -> return (x, [], in_))

staticPutStrLn :: String -> StaticIO ()
staticPutStrLn out = StaticIO (\in_ -> return ((), [out], in_))

instance LatteIO StaticIO where
    readInt = read <$> readString
    readString = staticGetLine
    error = fail "runtime error"
    printString = staticPutStrLn
