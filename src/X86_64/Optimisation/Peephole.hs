-- Optimisations based on matching the generated code to patterns of common
-- unoptimal code and transforming it to better versions.
{-# LANGUAGE QuasiQuotes #-}
module X86_64.Optimisation.Peephole (optimise) where

import           Data.Maybe
import           Text.RE.Replace
import           Text.RE.TDFA.String
import           Utilities

-- Applying one optimisation can make another one possible,
-- so we take the fixpoint of applying them continuously.
optimise :: [String] -> [String]
optimise = fixpoint runPeephole

runPeephole :: [String] -> [String]
runPeephole = runSize1 . runSize2 . runSize3

runSize1 :: [String] -> [String]
runSize1 = mapWithWindow1 size1Opts

runSize2 :: [String] -> [String]
runSize2 = mapWithWindow2 size2Opts

runSize3 :: [String] -> [String]
runSize3 = mapWithWindow3 size3Opts

size1Opts :: String -> Maybe [String]
size1Opts = foldr optComp optId [
        optSubZero,
        optAddZero,
        optCmpZero,
        optCmpZero2,
        optInc,
        optDec,
        optZero,
        optEmptyMov
    ]

size2Opts :: (String, String) -> Maybe [String]
size2Opts = foldr optComp optId [optJmpToNext]

size3Opts :: (String, String, String) -> Maybe [String]
size3Opts = foldr optComp optId [optXchg, optCmpJmp, optCondJmp]

optId :: a -> Maybe b
optId = const Nothing

optComp :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
optComp f g x = case g x of
    Just y  -> Just y
    Nothing -> f x

mapWithWindow1 :: (a -> Maybe [a]) -> [a] -> [a]
mapWithWindow1 f = concatMap (\l -> fromMaybe [l] (f l))

mapWithWindow2 :: ((a, a) -> Maybe [a]) -> [a] -> [a]
mapWithWindow2 _ [] = []
mapWithWindow2 _ [x] = [x]
mapWithWindow2 f (x1:x2:xs) =
    case f (x1, x2) of
        Nothing -> x1:mapWithWindow2 f (x2:xs)
        Just ys -> ys ++ mapWithWindow2 f xs

mapWithWindow3 :: ((a, a, a) -> Maybe [a]) -> [a] -> [a]
mapWithWindow3 _ [] = []
mapWithWindow3 _ [x] = [x]
mapWithWindow3 _ [x1, x2] = [x1, x2]
mapWithWindow3 f (x1:x2:x3:xs) =
    case f (x1, x2, x3) of
        Nothing -> x1:mapWithWindow3 f (x2:x3:xs)
        Just ys -> ys ++ mapWithWindow3 f xs

-- From:
--   movx $0, %a
-- To:
--   xorx %a, %a
optZero :: String -> Maybe [String]
optZero line =
    let pattern_ = [re|mov([a-z]) \$0, (%[^[:space:]]+)|]
    --                     capt1             capt2
        m = line ?=~ pattern_
    in if matched m
         then let (size, target) = extrMatch2 m
              in  Just ["  xor" ++ size ++ " " ++ target ++ ", " ++ target ++ matchSuf m]
         else Nothing

-- From:
--   addx $1, a
-- To:
--   incx a
optInc :: String -> Maybe [String]
optInc line =
    let pattern_ = [re|add([a-z]) \$1, ([^[:space:]]+)|]
    --                     capt1            capt2
        m = line ?=~ pattern_
    in if matched m
         then let (size, target) = extrMatch2 m
              in  Just ["  inc" ++ size ++ " " ++ target ++ matchSuf m]
         else Nothing

-- From:
--   subx $1, a
-- To:
--   decx a
optDec :: String -> Maybe [String]
optDec line =
    let pattern_ = [re|sub([a-z]) \$1, ([^[:space:]]+)|]
    --                     capt1            capt2
        m = line ?=~ pattern_
    in if matched m
         then let (size, target) = extrMatch2 m
              in  Just ["  dec" ++ size ++ " " ++ target ++ matchSuf m]
         else Nothing

-- From:
--   addx $0, a
-- To:
--   _
optAddZero :: String -> Maybe [String]
optAddZero line =
    let pattern_ = [re|add[a-z] \$0,|]
    in if matched  $ line ?=~ pattern_
         then Just []
         else Nothing

-- From:
--   subx $0, a
-- To:
--   _
optSubZero :: String -> Maybe [String]
optSubZero line =
    let pattern_ = [re|sub[a-z] \$0,|]
    in if matched  $ line ?=~ pattern_
         then Just []
         else Nothing

-- From:
--   cmpx $0, a
-- To:
--   testx a
optCmpZero :: String -> Maybe [String]
optCmpZero line =
    let pattern_ = [re|cmp([a-z]) \$0, ([^[:space:]]+)|]
    --                     capt1           capt2
        m = line ?=~ pattern_
    in if matched m
         then let (size, target) = extrMatch2 m
              in  Just ["  test" ++ size ++ " " ++ target ++ ", " ++ target ++ matchSuf m]
         else Nothing

-- From:
--   cmpx $0, a
-- To:
--   testx a
optCmpZero2 :: String -> Maybe [String]
optCmpZero2 line =
    let pattern_ = [re|cmp([a-z]) ([^[:space:]]+), \$0|]
    --                     capt1       capt2
        m = line ?=~ pattern_
    in if matched m
         then let (size, target) = extrMatch2 m
              in  Just ["  test" ++ size ++ " " ++ target ++ ", " ++ target ++ " " ++ matchSuf m]
         else Nothing

-- From:
--   movx a, a
-- To:
-- _
optEmptyMov :: String -> Maybe [String]
optEmptyMov line =
    let pattern_ = [re|mov[a-z] ([^[:space:]]+), ([^[:space:]]+)|]
    --                               capt1            capt2
        m = line ?=~ pattern_
    in if matched m
         then let (src, target) = extrMatch2 m
              in if src == target then Just [] else Nothing
         else Nothing

-- From:
--   jmp .L_label
-- .L_label:
-- To:
-- _
optJmpToNext :: (String, String) -> Maybe [String]
optJmpToNext (line1, line2) =
    let pattern1 = [re|jmp ([^[:space:]]+)|]
    --                         capt1
        pattern2 = [re|([^[:space:]]+):|]
    --                     capt1
        m1 = line1 ?=~ pattern1
        m2 = line2 ?=~ pattern2
    in if matched m1 && matched m2
         then let label1 = extrMatch m1
                  label2 = extrMatch m2
              in if label1 == label2 then Just [label1 ++ ":" ++ matchSuf m1 ++ matchSuf m2] else Nothing
         else Nothing

-- From:
--   setx a
--   testb a, a
--   jz .L_label
-- To:
--   j(!x) .L_label
optCmpJmp :: (String, String, String) -> Maybe [String]
optCmpJmp (line1, line2, line3) =
    let pattern1 = [re|set([a-z]) ([^[:space:]]+)|]
    --                     capt1      capt2
        pattern2 = [re|testb ([^[:space:]]+), ([^[:space:]]+)|]
    --                           capt1           capt2
        pattern3 = [re|jz ([^[:space:]]+)|]
    --                         capt1
        m1 = line1 ?=~ pattern1
        m2 = line2 ?=~ pattern2
        m3 = line3 ?=~ pattern3
    in
    if matched m1 && matched m2 && matched m3
      then let (cmp, target1) = extrMatch2 m1
               (target2_1, target2_2) = extrMatch2 m2
               label = extrMatch m3
           in if target1 == target2_1 && target2_1 == target2_2
                then Just ["  j" ++ negatedCond cmp ++ " " ++ label ++ concatMap matchSuf [m1, m2, m3]]
                else Nothing
      else Nothing

-- From:
--   jx .L_label
--   jmp .L_label2
-- .L_label:
-- To:
--   j(!x) .L_label2
-- .L_label:
optCondJmp :: (String, String, String) -> Maybe [String]
optCondJmp (line1, line2, line3) =
    let pattern1 = [re|j([a-z]) ([^[:space:]]+)|]
    --                   capt1       capt2
        pattern2 = [re|jmp ([^[:space:]]+)|]
    --                          capt1
        pattern3 = [re|([^[:space:]]+):|]
    --                      capt1
        m1 = line1 ?=~ pattern1
        m2 = line2 ?=~ pattern2
        m3 = line3 ?=~ pattern3
    in
    if matched m1 && matched m2 && matched m3
      then let (cmp, target1) = extrMatch2 m1
               target2        = extrMatch m2
               label          = extrMatch m3
           in if target1 == label
                then Just ["  j" ++ negatedCond cmp ++ " " ++ target2 ++ concatMap matchSuf [m1, m2],
                           label ++ ":" ++ matchSuf m3]
                else Nothing
      else Nothing

-- From:
--   movx a, c
--   movx b, a
--   movx c, b
-- To:
--   xchgx a, b
optXchg :: (String, String, String) -> Maybe [String]
optXchg (line1, line2, line3) =
    let pattern_ = [re|mov([a-z]) ([^[:space:]]+), ([^[:space:]]+)|]
    --                     capt1       capt2            capt3
    in case (line1 ?=~ pattern_, line2 ?=~ pattern_, line3 ?=~ pattern_) of
        (m1, m2, m3) | matched m1 &&
                          matched m2 &&
                          matched m3 ->
            let (s1, l1, r1) = extrMatch3 m1
                (s2, l2, r2) = extrMatch3 m2
                (s3, l3, r3) = extrMatch3 m3
            in if s1 == s2 && s2 == s3 &&
                  l1 == r2 && l2 == r3 && r1 == l3
               then Just ["  xchg" ++ s1 ++ " " ++ l1 ++ ", " ++ l2 ++ " " ++ concatMap matchSuf [m1, m2, m3]]
               else Nothing
        _ -> Nothing

extrMatch :: Match a -> a
extrMatch m = capturedText $ m !$ [cp|1|]

extrMatch2 :: Match a -> (a, a)
extrMatch2 m = (capturedText $ m !$ [cp|1|], capturedText $ m !$ [cp|2|])

extrMatch3 :: Match a -> (a, a, a)
extrMatch3 m = (capturedText $ m !$ [cp|1|],
                capturedText $ m !$ [cp|2|],
                capturedText $ m !$ [cp|3|])

matchSuf :: Match String -> String
matchSuf m = captureSuffix $ m !$ [cp|0|]

negatedCond :: String -> String
negatedCond x = case x of
            "g"  -> "le"
            "ge" -> "l"
            "l"  -> "ge"
            "le" -> "l"
            "e"  -> "ne"
            "ne" -> "e"
            "z"  -> "nz"
            "nz" -> "z"
            _    -> error $ "negatedCond: invalid condition " ++ x
