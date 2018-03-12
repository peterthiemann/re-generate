{-# LANGUAGE OverloadedStrings #-}
module GenString where

import GRegexp
import LLeq
import OrderedLists
import Partitions
import Types

import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import qualified Data.List (intersect)
import qualified Data.Text as T

-- | inefficient definition due to use of snoc
sigma_star :: Sigma -> Lang
sigma_star sigma =
    loop
    where
      loop = T.empty : concatMap f loop
      snoc xs x = xs <> T.singleton x
      f ts = map (snoc ts) sigma

-- | create sigma* in a segmentized way avoids the inefficiency of snoc
sigma_star' :: Sigma -> Lang
sigma_star' sigma =
    concat segments
    where
      segments = [T.empty] : map extend segments
      extend segment = concatMap (\x -> map (T.singleton x<>) segment) sigma

-- | has problems because it may hang
concatenate ::Lang -> Lang -> Lang
concatenate xss yss = collect 0
  where
    xsegs = segmentize xss
    ysegs = segmentize yss
    collect n =
        (multimerge $ map (combine n) [0 .. n]) <> collect (n+1)
    combine n i =
        concatMap (\xs -> map (\ys -> xs <> ys) (ysegs !! (n - i))) (xsegs !! i)

-- | for testing
xss = ["", "a", "cd"]
yss = ["", "b", "aa"]

xsegs = segmentize xss
ysegs = segmentize yss
combine n i = concatMap (\xs -> map (\ys -> xs <> ys) (ysegs !! (n - i))) (xsegs !! i)


-- | collect elements of the same length; always returns an infinite list
-- each same-length segment is sorted lexicographically
segmentize :: Lang -> [[T.Text]]
segmentize = collect 0
  where
    collect n xss =
        let (takes, drops) =
                splitWhile (\xs -> T.length xs == n) xss
        in takes : collect (n+1) drops

-- | declarative, but not productive
star xss = merge [T.empty] (concatenate xss (star xss))

-- | generate elements of the language of the gre as an ll-ascending stream
generate :: Sigma -> GRE Char -> Lang
generate sigma r = gen r
  where
    gen Zero = []
    gen One  = [T.empty]
    gen (Atom t) = [T.singleton t]
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = merge (gen r) (gen s)
    gen (And r s) = intersect (gen r) (gen s)
    gen (Not r) = difference (sigma_star sigma) (gen r)
    gen (Star r) = star (gen r)

-- fix problems with concat

-- collect elements of the same length; returns a finite list for finite languages
segmentize' :: Lang -> [[T.Text]]
segmentize' = collect 0
  where
    collect n [] = []
    collect n xss =
        let (takes, drops) =
                splitWhile (\xs -> T.length xs == n) xss
        in takes : collect (n+1) drops

concatenate' :: Lang -> Lang -> Lang
concatenate' xss yss = collect 0
  where
    xsegs = segmentize' xss
    ysegs = segmentize' yss
    exhausted xs n = all isNothing (map (maybeIndex xs) [n `div` 2 .. n])
    collect n | exhausted xsegs n && exhausted ysegs n = []
              | otherwise = (multimerge $ map (combine n) [0 .. n]) <> collect (n+1)
    combine n i = concatMap (\xs -> map (\ys -> xs <> ys) (fromMaybe [] $ maybeIndex ysegs (n - i))) (fromMaybe [] $ maybeIndex xsegs i)

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex [] _ =
    Nothing
maybeIndex (x:xs) n =
    if n == 0 then Just x else maybeIndex xs (n-1)

-- | declarative, but not productive
star' xss = merge [T.empty] (concatenate' xss (star' xss))


-- | not generally productive
star4 :: [T.Text] -> [T.Text]
star4 xss = collect 0
  where
    xsegs = segmentize xss
    collect n =
        (multimerge $ map wordsFromPartition (partitions n)) <> collect (n + 1)
    wordsFromPartition [] = [T.empty]
    wordsFromPartition (i:is) =
        concatMap (\w -> map (<>w) (xsegs !! i)) (wordsFromPartition is)

-- | productive
star4' :: [T.Text] -> [T.Text]
star4' xss = T.empty : collect 1
  where
    xsegs = segmentize xss
    infiniteResult = any (\xs -> T.length xs > 0) xss
    collect :: Int -> Lang
    collect n
      | infiniteResult =
            (multimerge $ map wordsFromPartition (partitions n)) <> collect (n + 1)
      | otherwise = []
    wordsFromPartition [] = [T.empty]
    wordsFromPartition (i:is) =
        concatMap (\w -> map (<>w) (xsegs !! i)) (wordsFromPartition is)

-- | productive, more efficient?
star5 :: [T.Text] -> [T.Text]
star5 xss = T.empty : collect 1
  where
    xsegs = segmentize xss
    emptysegs = map (not . null) xsegs
    indexesOfNonEmptysegs n = map snd $ filter fst $ zip emptysegs [0 .. n]
    infiniteResult = any (\xs -> T.length xs > 0) xss
    collect n
      | infiniteResult = (multimerge $ map wordsFromPartition (restrictedPartitions (indexesOfNonEmptysegs n) n)) ++ collect (n + 1)
      | otherwise = []
    wordsFromPartition [] = [T.empty]
    wordsFromPartition (i:is) = concatMap (\w -> map (w<>) (xsegs !! i)) (wordsFromPartition is)

-- | computing the indexesOfNonEmptysegs by accumulation
star6 :: [T.Text] -> [T.Text]
star6 xss = T.empty : collect (tail xsegs) [] 1
  where
    xsegs = segmentize xss
    infiniteResult =
        any (\xs -> T.length xs > 0) xss
    -- indexesOfNonEmptysegs is sorted decreasingly: taking advantage of that in restrictedPartitions'!
    collect (segn : segs) indexesOfNonEmptysegs n
      | infiniteResult =
          let indexesOfNonEmptysegs' =
                  if null segn
                  then indexesOfNonEmptysegs
                  else n : indexesOfNonEmptysegs
          in (multimerge $ map wordsFromPartition (restrictedPartitions' indexesOfNonEmptysegs' n))
                         <> collect segs indexesOfNonEmptysegs' (n + 1)
      | otherwise = []
    wordsFromPartition [] = [T.empty]
    wordsFromPartition (i:is) = concatMap (\w -> map (w<>) (xsegs !! i)) (wordsFromPartition is)



-- | pn = restrictedPartitions ns n
-- xs \in pn => sum xs = n, xi \in xs => xi \in ns /\ xi > 0
-- no repetitions
-- repeated filtering may be O(n^2)
restrictedPartitions :: [Int] -> Int -> [[Int]]
restrictedPartitions [] n = [[]]
restrictedPartitions ns n
  | n == 0 = [[]]
  | otherwise = let ns' = filter (<=n) ns in concatMap (\i -> map (i:) (restrictedPartitions ns' (n - i))) ns'


collect n = concatMap wordsFromPartition (partitions n)

wordsFromPartition [] = [T.empty]
wordsFromPartition (i:is) = concatMap (\w -> map (<>w) (xsegs !! i)) (wordsFromPartition is)

-- | check if list is ll sorted
llsorted [] = []
llsorted [_] = []
llsorted (x:xs@(y:_)) = lleq x y : llsorted xs

-- | combination of takeWhile and dropWhile
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p [] = ([], [])
splitWhile p xs@(x:xs')
  | p x = let (takes, drops) = splitWhile p xs' in (x:takes, drops)
  | otherwise = ([], xs)

-- | generate elements of the language of the gre as an ll-ascending stream; the final thing
generate' :: Sigma -> GRE Char -> Lang
generate' sigma r = gen r
  where
    gen Zero = []
    gen One  = [T.empty]
    gen (Atom t) = [T.singleton t]
    gen (Dot r s) = concatenate' (gen r) (gen s)
    gen (Or r s) = merge (gen r) (gen s)
    gen (And r s) = intersect (gen r) (gen s)
    gen (Not r) = difference (sigma_star' sigma) (gen r)
    gen (Star r) = star6 (gen r)
