module X86_64.Stack where

import           Data.List   (delete, minimumBy, sort)
import           X86_64.Size

type Hole = (Int, Int)

data Stack = Stack {stackContents :: [Hole], stackSize :: Int} deriving Show

emptyStack :: Stack
emptyStack = Stack [] 0

stackPush :: Size -> Stack -> (Stack, Int)
stackPush size stack =
    case findHole size stack of
        Nothing -> (stack {stackSize = stackSize stack + sizeInBytes size}, -stackSize stack - sizeInBytes size)
        Just h@(f, _) -> (shrinkHole h size stack, -f - sizeInBytes size)

stackRemove :: Int -> Size -> Stack -> Stack
stackRemove offset size = addHole (-offset - sizeInBytes size, -offset)

addHole :: Hole -> Stack -> Stack
addHole h stack = stackNormalise $ stack { stackContents = h:stackContents stack }

shrinkHole :: Hole -> Size -> Stack -> Stack
shrinkHole h@(f, t) size stack =
    let h' = (f, t - sizeInBytes size)
    in  stackNormalise $ stack { stackContents = h':delete h (stackContents stack)}

findHole :: Size -> Stack -> Maybe Hole
findHole size stack =
    let valid = filter (\h -> holeSize h > sizeInBytes size) (stackContents stack)
    in  safeMinimumBy cmp valid
    where safeMinimumBy _ [] = Nothing
          safeMinimumBy f xs = Just $ minimumBy f xs
          cmp h1 h2 = compare (holeSize h1) (holeSize h2)

holeSize :: Hole -> Int
holeSize (f, t) = t - f + 1

stackNormalise :: Stack -> Stack
stackNormalise stack = stack { stackContents = foldr normaliseHoles [] (sort $ stackContents stack)}
    where
        normaliseHoles x [] = [x]
        normaliseHoles (f2, t2) ((f1, t1):hs) | t2 >= f1 = (f2, max t1 t2):hs
        normaliseHoles h hs = h:hs
