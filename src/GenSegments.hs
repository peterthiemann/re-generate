module GenSegments where

import GRegexp
import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.Maybe
import qualified Data.Map.Strict as Map

type Segments t = [Lang t]

-- | length lexicographic ordering
lleq :: (Ord t) => [t] -> [t] -> Bool
lleq xs ys =
  let lxs = length xs
      lys = length ys
  in  lxs < lys ||
      lxs == lys && xs <= ys

merge :: (Ord t) => Lang t -> Lang t -> Lang t
merge [] yss = yss
merge xss [] = xss
merge xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = xs : merge xss' yss'
  | lleq xs ys = xs : merge xss' yss
  | otherwise  = ys : merge xss yss'

intersect :: (Ord t) => Lang t -> Lang t -> Lang t
intersect [] yss = []
intersect xss [] = []
intersect xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = xs : intersect xss' yss'
  | lleq xs ys = intersect xss' yss
  | otherwise  = intersect xss yss'

difference :: (Ord t) => Lang t -> Lang t -> Lang t
difference [] yss = []
difference xss [] = xss
difference xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = difference xss' yss'
  | lleq xs ys = xs : difference xss' yss
  | otherwise  = difference xss yss'

-- for infinite lists of segments
mergeSegs :: (Ord t) => Segments t -> Segments t -> Segments t
mergeSegs = zipWith merge

intersectSegs :: (Ord t) => Segments t -> Segments t -> Segments t
intersectSegs = zipWith intersect

differenceSegs :: (Ord t) => Segments t -> Segments t -> Segments t
differenceSegs = zipWith difference

multimerge :: (Ord t) => [Lang t] -> Lang t
multimerge = foldr merge []

sigmaStarSegs :: [t] -> Segments t
sigmaStarSegs sigma = segments
  where
    segments = [[]] : map extend segments
    extend segment = concatMap (\x -> map (x:) segment) sigma

concatenate :: (Ord t) => Segments t -> Segments t -> Segments t
concatenate xsegs ysegs = collect 0
  where
    collect n = (multimerge $ map (combine n) [0 .. n]) : collect (n+1)
    combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (listIndex ysegs (n - i)))
                            (listIndex xsegs i)

listIndex :: [[a]] -> Int -> [a]
listIndex [] _ = []
listIndex (x:xs) n = if n == 0 then x else listIndex xs (n-1)

-- the star operation

-- | computing the indexesOfNonEmptysegs by accumulation
star :: (Ord t) => Segments t -> Segments t
star xsegs = [[]] : collect Map.empty (tail xsegs) [] 1
  where
    collect mappedSegs (segn : segs) indexesOfNonEmptysegs n = 
          let indexesOfNonEmptysegs' = if null segn then indexesOfNonEmptysegs else n : indexesOfNonEmptysegs
              mappedSegs' = Map.insert n segn mappedSegs
          in 
             (multimerge $ map (wordsFromPartition mappedSegs') (restrictedPartitions' indexesOfNonEmptysegs' n))
                         : collect mappedSegs' segs indexesOfNonEmptysegs' (n + 1)

    wordsFromPartition :: Map.Map Int (Lang t) -> [Int] -> Lang t
    wordsFromPartition msegs [] = [[]]
    wordsFromPartition msegs (i:is) = concatMap (\w -> map (w++) (msegs Map.! i)) (wordsFromPartition msegs is)

-- | pn = restrictedPartitions ns n
-- xs \in pn => sum xs = n, xi \in xs => xi \in ns /\ xi > 0
-- no repetitions
-- ns is sorted decreasingly
restrictedPartitions' :: [Int] -> Int -> [[Int]]
restrictedPartitions' [] n = [[]]
restrictedPartitions' ns n
  | n == 0 = [[]]
  | otherwise = let ns' = dropWhile (>n) ns in concatMap (\i -> map (i:) (restrictedPartitions' ns' (n - i))) ns'

-- | generate elements of the language of the gre as a stream of segments
generate' :: (Ord t) => [t] -> GRE t -> Segments t
generate' sigma r = gen r
  where
    gen Zero = repeat []
    gen One  = [[]] : repeat []
    gen (Atom t) = [] : [[t]] : repeat []
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = mergeSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = differenceSegs (sigmaStarSegs sigma) (gen r)
    gen (Star r) = star (gen r)

generate :: (Ord t) => [t] -> GRE t -> Lang t
generate sigma = concat . generate' sigma

