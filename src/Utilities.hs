module Utilities where

import           Control.Monad (unless)
import           Data.Foldable
import           Data.List     (sort, sortOn)
import qualified Data.Map      as Map

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = do
  b <- p
  unless b a

-- Find duplicates in a given list based on a key selector.
-- Returns a deduplicated list of duplicated keys and a list of values such that
-- there exists a value with the same key.
findDupsBy :: Ord k => (a -> k) -> [a] -> ([k], [a])
findDupsBy f ds = collect $ foldr checkForDup (Map.empty, []) ds
    where
    checkForDup a (m, dups) =
        let k = f a
        in  if Map.member k m then (m, (k, a) : dups) else (Map.insert k a m, dups)
    collect (m, dups) =
        let (ks, as) = unzip dups in (ks, foldr (\k as' -> m Map.! k : as') as ks)

-- Inner join based on a key selector of two lists.
-- Returns a deduplicated list of keys that participated in a join
-- and the list of resulting products.
findConflictsBy :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> ([k], [(a, b)])
findConflictsBy fa fb as bs = unzip $ foldr checkForConfl [] bs
    where
    m = Map.fromList $ zip (map fa as) as
    checkForConfl b confls =
        let k = fb b
        in  case Map.lookup k m of
                Nothing -> confls
                Just a  -> (k, (a, b)) : confls

-- O(nlogn) deduplication.
dedup :: Ord a => [a] -> [a]
dedup xs = run (sort xs)
    where run []      = []
          run (x:xs') = x : run (dropWhile (== x) xs' )

-- O(nlogn) deduplication by key.
dedupBy :: Ord k => (a -> k) -> [a] -> [a]
dedupBy f xs = run (sortOn fst $ map (\x -> (f x, x)) xs)
    where run []           = []
          run ((k, x):xs') = x : run (dropWhile ((== k) . fst) xs')

single :: (Foldable f) => f a -> Maybe a
single = find (const True)
