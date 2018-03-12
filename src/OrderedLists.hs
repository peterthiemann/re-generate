module OrderedLists (
  merge, intersect, difference, mergeSegs, intersectSegs, differenceSegs, multimerge
  ) where

import Types

import qualified Data.Text as T

merge :: Lang -> Lang -> Lang
merge [] yss = yss
merge xss [] = xss
merge xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = xs : merge xss' yss'
  | xs <= ys = xs : merge xss' yss
  | otherwise  = ys : merge xss yss'

intersect :: Lang -> Lang -> Lang
intersect [] yss = []
intersect xss [] = []
intersect xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = xs : intersect xss' yss'
  | xs <= ys = intersect xss' yss
  | otherwise  = intersect xss yss'

difference :: Lang -> Lang -> Lang
difference [] yss = []
difference xss [] = xss
difference xss@(xs:xss') yss@(ys:yss')
  | xs == ys   = difference xss' yss'
  | xs <= ys = xs : difference xss' yss
  | otherwise  = difference xss yss'

-- for infinite lists of segments
mergeSegs :: Segments -> Segments -> Segments
mergeSegs = zipWith merge

intersectSegs :: Segments -> Segments -> Segments
intersectSegs = zipWith intersect

differenceSegs :: Segments -> Segments -> Segments
differenceSegs = zipWith difference

multimerge :: [Lang] -> Lang
multimerge = foldr merge []
