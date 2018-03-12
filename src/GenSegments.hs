module GenSegments where

import GRegexp
import LLeq
import OrderedLists
import Partitions
import Types

import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import qualified Data.List (intersect)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

sigmaStarSegs :: Sigma -> Segments
sigmaStarSegs sigma =
    segments
    where
      segments = [T.empty] : map extend segments
      extend segment = concatMap (\x -> map (T.singleton x<>) segment) sigma

concatenate :: Segments -> Segments -> Segments
concatenate xsegs ysegs = collect xsegs ysegs Map.empty Map.empty [] [] 0
  where
    collect (xseg:xsegs) (yseg:ysegs) xmap ymap xneidxs yneidxs n =
      let xmap' = Map.insert n xseg xmap
          ymap' = Map.insert n yseg ymap
          xneidxs' = if null xseg then xneidxs else n : xneidxs
          yneidxs' = if null yseg then yneidxs else n : yneidxs
          combine i =
              concatMap (\xs -> map (\ys -> xs <> ys) (ymap' Map.! (n - i))) (xmap' Map.! i)
          usefulxidxs = filter (\i -> (n-i) `elem` yneidxs') xneidxs'
      in
        (multimerge $ map combine usefulxidxs)
        : collect xsegs ysegs xmap' ymap' xneidxs' yneidxs' (n+1)

-- the star operation

-- | computing the indexesOfNonEmptysegs by accumulation
star :: Segments -> Segments
star xsegs = [T.empty] : collect Map.empty (tail xsegs) [] 1
  where
    collect mappedSegs (segn : segs) indexesOfNonEmptysegs n =
          let indexesOfNonEmptysegs' = if null segn then indexesOfNonEmptysegs else n : indexesOfNonEmptysegs
              mappedSegs' = Map.insert n segn mappedSegs
          in
             (multimerge $ map (wordsFromPartition mappedSegs') (restrictedPartitions' indexesOfNonEmptysegs' n))
                         : collect mappedSegs' segs indexesOfNonEmptysegs' (n + 1)

wordsFromPartition :: Map.Map Int (Lang) -> [Int] -> Lang
wordsFromPartition msegs [] = [T.empty]
wordsFromPartition msegs (i:is) = concatMap (\w -> map (w<>) (msegs Map.! i)) (wordsFromPartition msegs is)

complementSegs :: Sigma -> Segments -> Segments
complementSegs sigma = differenceSegs (sigmaStarSegs sigma)

-- | generate elements of the language of the gre as a stream of segments
generate' :: Sigma -> GRE Char -> Segments
generate' sigma r = gen r
  where
    gen Zero = repeat []
    gen One  = [T.empty] : repeat []
    gen (Atom t) = [] : [T.singleton t] : repeat []
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = mergeSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star (gen r)

generate :: Sigma -> GRE Char -> Lang
generate sigma = concat . generate' sigma
