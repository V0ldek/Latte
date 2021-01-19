module X86_64.RegisterAllocation.RandomSequence where

import           Data.List
import qualified Data.Map                                    as Map
import           System.Random                               (mkStdGen, random)
import           X86_64.RegisterAllocation.InterferenceGraph

randomSequence :: InterferenceGraph -> [String]
randomSequence (IG g) =
    let ns = Map.keys g
        size = length ns
        rands = randomNumbers size
    in map fst $ sortOn snd $ zip ns rands

randomNumbers :: Int -> [Int]
randomNumbers x = snd $ mapAccumL (\g _ -> swap (random  g)) (mkStdGen 29794820) [0..x]

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
