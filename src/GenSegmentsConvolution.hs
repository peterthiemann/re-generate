module GenSegmentsConvolution where

import GRegexp
import OrderedLists
import Partitions
import Types

import Control.Applicative
import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import qualified Data.List (intersect)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

sigmaStarSegs :: Alphabet -> Segments
sigmaStarSegs sigma =
    segments
    where
      segments = [mempty] : map extend segments
      extend = liftA2 T.cons sigma

concatenate :: Segments -> Segments -> Segments
concatenate lx ly =
  collect lx ly Nothing Nothing [] 0
    where
      collect xss yss mmx mmy ryss n =
        let (xs, xss', mmx') = updateMax xss mmx n
            (ys, yss', mmy') = updateMax yss mmy n
            mbound = liftA2 (+) mmx mmy
            ryss' = ys : ryss
        in
          case mbound of
            Just m | n >= m - 1 ->
                     []
            _ ->
              (multimerge $ zipWith (liftA2 T.append) lx ryss')
              : collect xss' yss' mmx' mmy' ryss' (n+1)
      updateMax _ mm@(Just _) n = ([], [], mm)
      updateMax [] Nothing n = ([], [], Just n)
      updateMax (xs:xss) Nothing n = (xs, xss, Nothing)


-- | the star operation
star :: Segments -> Segments
star [] = [[mempty]]
star [_] = [[mempty]]
star (xseg0:xsegs) = ysegs
  where
    ysegs = [mempty] : collect ysegs []
    collect (ysegi : ysegs) rsegs =
      let rsegs' = ysegi : rsegs in
        (multimerge $ zipWith (liftA2 T.append) xsegs rsegs')
        : collect ysegs rsegs'

-- | generate elements of the language of the gre as a stream of segments
generate' :: Alphabet -> GRE Char -> Segments
generate' sigma r = gen r
  where
    sigmaStar = sigmaStarSegs sigma

    gen Zero = []
    gen One  = [[mempty]]
    gen (Atom t) = [[], [T.singleton t]]
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = mergeSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = differenceSegs sigmaStar (gen r)
    gen (Star r) = star (gen r)

generate :: Alphabet -> GRE Char -> Lang
generate sigma = concat . generate' sigma
