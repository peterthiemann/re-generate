module OrderedLists (
  merge, intersect, difference
  ) where

import LLeq

type Lang t = [[t]]

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

