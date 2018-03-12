module GenSegmentsStar2 where

import GRegexp
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
      segments = [mempty] : map extend segments
      extend segment = concatMap (\x -> map (T.singleton x<>) segment) sigma

concatenate :: Segments -> Segments -> Segments
concatenate xsegs ysegs = collect xsegs ysegs Map.empty Map.empty [] [] 0
  where
    collect (xseg:xsegs) (yseg:ysegs) xmap ymap xneidxs yneidxs n =
      let xmap' = Map.insert n xseg xmap
          ymap' = Map.insert n yseg ymap
          xneidxs' = if null xseg then xneidxs else n : xneidxs
          yneidxs' = if null yseg then yneidxs else n : yneidxs
          combine i = concatMap (\xs -> map (\ys -> xs <> ys) (ymap' Map.! (n - i))) (xmap' Map.! i)
          usefulxidxs = filter (\i -> (n-i) `elem` yneidxs') xneidxs'
      in
        (multimerge $ map combine usefulxidxs)
        : collect xsegs ysegs xmap' ymap' xneidxs' yneidxs' (n+1)

-- the star operation

star' :: Segments -> Segments
star' xsegs = rsegs
  where
    rsegs = [mempty] : collect (tail xsegs) rsegs Map.empty Map.empty [] [] 1
    collect (xsegn : xsegs) (rsegn1 : rsegs) xmap rmap xneidxs rneidxs n =
      let xmap' = Map.insert n xsegn xmap
          rmap' = Map.insert (n-1) rsegn1 rmap
          xneidxs' = if null xsegn then xneidxs else n : xneidxs
          rneidxs' = if null rsegn1 then rneidxs else (n-1) : rneidxs
          combine i = concatMap (\xs -> map (xs <> ) (rmap' Map.! (n - i))) (xmap' Map.! i)
          usefulxidxs = filter (\i -> (n-i) `elem` rneidxs') xneidxs'
      in
        (multimerge $ map combine usefulxidxs)
        : collect xsegs rsegs xmap' rmap' xneidxs' rneidxs' (n+1)

complementSegs :: Sigma -> Segments -> Segments
complementSegs sigma = differenceSegs (sigmaStarSegs sigma)

-- | generate elements of the language of the gre as a stream of segments
generate' :: Sigma -> GRE Char -> Segments
generate' sigma r = gen r
  where
    gen Zero = repeat []
    gen One  = [mempty] : repeat []
    gen (Atom t) = [] : [T.singleton t] : repeat []
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = mergeSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star' (gen r)

generate :: Sigma -> GRE Char -> Lang
generate sigma = concat . generate' sigma
