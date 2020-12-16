-- Monoid based control flow status definition and combinators.
module SemanticAnalysis.ControlFlow where

import           Control.Monad

newtype Reachability = Reach Bool

-- A given code branch is reachable if either of the paths leading to it is reachable.
instance Semigroup Reachability where
    (Reach b1) <> (Reach b2) = Reach (b1 || b2)

-- False is the neutral element of ||.
instance Monoid Reachability where
    mempty = Reach False

class ControlFlow m where
    getReach :: m Reachability
    setReach :: Reachability -> m ()

isReachable :: Reachability -> Bool
isReachable (Reach b) = b

-- Analyse a given scoped statement preserving the reachability status
-- and returning the reachability status of the end of the analysed scoped statement.
controlScope :: (Monad m, ControlFlow m) => m a -> m (a, Reachability)
controlScope m = do
    before <- getReach
    a <- m
    after <- getReach
    setReach before
    return (a, after)

-- Analyse a given block that may be entered or may be not entered.
mayEnter :: (Monad m, ControlFlow m) => m a -> m a
mayEnter m = do
    x <- mayEnterOneOf [m]
    return $ head x

-- Analyse a sequence of statements assuming that either one or none of them
-- will be entered.
mayEnterOneOf :: (Monad m, ControlFlow m) => [m a] -> m [a]
mayEnterOneOf ms = do
    (as, _) <- mapAndUnzipM controlScope ms
    return as

-- Analyse a statement assuming it will always be entered.
mustEnter :: (Monad m, ControlFlow m) => m a -> m a
mustEnter m = do
    x <- mustEnterOneOf [m]
    return $ head x

-- Analyse a pair of statements assuming the first one will always be entered
-- and the second one will never be entered.
mustEnterFirst :: (Monad m, ControlFlow m) => m a -> m a -> m (a, a)
mustEnterFirst m1 m2 = do
    (a2, _) <- controlScope m2
    a1 <- mustEnter m1
    return (a1, a2)

-- Analyse a pair of statements assuming the first one will never be entered
-- and the second one will always be entered.
mustEnterSecond :: (Monad m, ControlFlow m) => m a -> m a -> m (a, a)
mustEnterSecond m1 m2 = do
    (a1, _) <- controlScope m1
    a2 <- mustEnter m2
    return (a1, a2)

-- Analyse a pair of statements assuming that exactly one of them will be entered
-- and the other will not.
mustEnterOneOf2 :: (Monad m, ControlFlow m) => m a -> m a -> m (a, a)
mustEnterOneOf2 m1 m2 = do
    x <- mustEnterOneOf [m1, m2]
    return (head x, x !! 1)

-- Analyse a sequuence of statements assuming that exactly one of them will be entered
-- and none of the others will.
mustEnterOneOf :: (Monad m, ControlFlow m) => [m a] -> m [a]
mustEnterOneOf ms = do
    (as, reaches) <- mapAndUnzipM controlScope ms
    setReach (mconcat reaches)
    return as
