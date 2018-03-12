module GenNaiveStar where

import GRegexp
import OrderedLists
import Partitions
import Types

import Data.List hiding (intersect)
import Data.Monoid
import qualified Data.List (intersect)
import qualified Data.Text as T

sigmaStarSegs :: Sigma -> Segments
sigmaStarSegs sigma =
    segments
    where
      segments = [T.empty] : map extend segments
      extend segment = concatMap (\x -> map (T.singleton x<>) segment) sigma

concatenate :: Segments -> Segments -> Segments
concatenate xsegs ysegs =
    collect 0
    where
      collect n =
          let combine i =
                  concatMap (\xs -> map ( xs <> ) (ysegs !! (n - i))) (xsegs !! i)
          in  (multimerge $ map combine [0 .. n]) : collect (n+1)


-- the star operation

-- | computing the indexesOfNonEmptysegs by accumulation
star :: Segments -> Segments
star xsegs = rsegs
  where
    rsegs = [T.empty] : collect 1
    collect n =
      let combine i = concatMap (\xs -> map (xs<>) (rsegs !! (n - i))) (xsegs !! i)
      in  (multimerge $ map combine [1 .. n]) : collect (n + 1)

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
