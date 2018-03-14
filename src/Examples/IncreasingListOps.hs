module Examples.IncreasingListOps where

union :: (Ord t) => [t] -> [t] -> [t]
union xs@(x:xs') ys@(y:ys') =
  case compare x y of
    EQ -> x : union xs' ys'
    LT -> x : union xs' ys
    GT -> y : union xs ys'
union xs ys = xs ++ ys

intersect :: (Ord t) => [t] -> [t] -> [t]
intersect xs@(x:xs') ys@(y:ys') =
  case compare x y of
    EQ -> x : intersect xs' ys'
    LT -> intersect xs' ys
    GT -> intersect xs ys'
intersect xs ys = []

difference :: (Ord t) => [t] -> [t] -> [t]
difference xs@(x:xs') ys@(y:ys') =
  case compare x y of
    EQ -> difference xs' ys'
    LT -> x : difference xs' ys
    GT -> difference xs ys'
difference xs ys = xs
