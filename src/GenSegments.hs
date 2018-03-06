module GenSegments where

import GRegexp
import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.Maybe

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
  | xs == ys = xs : merge xss' yss'
  | lleq xs ys = xs : merge xss' yss
  | otherwise    = ys : merge xss yss'

intersect :: (Ord t) => Lang t -> Lang t -> Lang t
intersect [] yss = []
intersect xss [] = []
intersect xss@(xs:xss') yss@(ys:yss')
  | xs == ys = xs : intersect xss' yss'
  | lleq xs ys = intersect xss' yss
  | otherwise    = intersect xss yss'

difference :: (Ord t) => Lang t -> Lang t -> Lang t
difference [] yss = []
difference xss [] = xss
difference xss@(xs:xss') yss@(ys:yss')
  | xs == ys = difference xss' yss'
  | lleq xs ys = xs : difference xss' yss
  | otherwise    = difference xss yss'

-- for infinite lists of segments
mergeSegs :: (Ord t) => Segments t -> Segments t -> Segments t
mergeSegs = zipWith merge

intersectSegs :: (Ord t) => Segments t -> Segments t -> Segments t
intersectSegs = zipWith intersect

differenceSegs :: (Ord t) => Segments t -> Segments t -> Segments t
differenceSegs = zipWith difference

multimerge = foldr mergeSegs (repeat [])

sigmaStarSegs :: [t] -> Segments t
sigmaStarSegs sigma = segments
  where
    segments = [[]] : map extend segments
    extend segment = concatMap (\x -> map (x:) segment) sigma

concatenate :: (Ord t) => Segments t -> Segments t -> Segments t
concatenate xsegs ysegs = collect 0
  where
    exhausted xs n = all isNothing (map (maybeIndex xs) [n `div` 2 .. n])
    collect n | exhausted xsegs n && exhausted ysegs n = []
              | otherwise = (multimerge $ map (combine n) [0 .. n]) ++ collect (n+1)
    combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (fromMaybe [] $ maybeIndex ysegs (n - i))) (fromMaybe [] $ maybeIndex xsegs i)

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex [] _ = Nothing
maybeIndex (x:xs) n = if n == 0 then Just x else maybeIndex xs (n-1)

