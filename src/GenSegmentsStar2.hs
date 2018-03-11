module GenSegmentsStar2 where

import GRegexp
import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.Maybe
import qualified Data.Map.Strict as Map

import LLeq
import OrderedLists
import Partitions

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

star' :: (Ord t) => Segments t -> Segments t
star' xsegs = rsegs
  where
    rsegs = [[]] : collect (tail xsegs) rsegs Map.empty Map.empty [] [] 1
    collect (xsegn : xsegs) (rsegn1 : rsegs) xmap rmap xneidxs rneidxs n =
      let xmap' = Map.insert n xsegn xmap
          rmap' = Map.insert (n-1) rsegn1 rmap
          xneidxs' = if null xsegn then xneidxs else n : xneidxs
          rneidxs' = if null rsegn1 then rneidxs else (n-1) : rneidxs
          combine i = concatMap (\xs -> map (xs ++ ) (rmap' Map.! (n - i))) (xmap' Map.! i)
          usefulxidxs = filter (\i -> (n-i) `elem` rneidxs') xneidxs'
      in
        (multimerge $ map combine usefulxidxs)
        : collect xsegs rsegs xmap' rmap' xneidxs' rneidxs' (n+1)

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
    gen (Star r) = star' (gen r)

generate :: (Ord t) => [t] -> GRE t -> Lang t
generate sigma = concat . generate' sigma

