module GenSegments where

import GRegexp
import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.Maybe
import qualified Data.Map.Strict as Map

import LLeq
import OrderedLists

type Segments t = [Lang t]

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
concatenate xsegs ysegs = collect xsegs ysegs Map.empty Map.empty [] [] 0
  where
    collect (xseg:xsegs) (yseg:ysegs) xmap ymap xneidxs yneidxs n =
      let xmap' = Map.insert n xseg xmap
          ymap' = Map.insert n yseg ymap
          xneidxs' = if null xseg then xneidxs else n : xneidxs
          yneidxs' = if null yseg then yneidxs else n : yneidxs
          combine i = concatMap (\xs -> map (\ys -> xs ++ ys) (ymap' Map.! (n - i))) (xmap' Map.! i)
          usefulxidxs = filter (\i -> (n-i) `elem` yneidxs') xneidxs'
      in
        (multimerge $ map combine usefulxidxs)
        : collect xsegs ysegs xmap' ymap' xneidxs' yneidxs' (n+1)

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

complementSegs :: (Ord t) => [t] -> Segments t -> Segments t
complementSegs sigma = differenceSegs (sigmaStarSegs sigma)

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
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star (gen r)

generate :: (Ord t) => [t] -> GRE t -> Lang t
generate sigma = concat . generate' sigma

