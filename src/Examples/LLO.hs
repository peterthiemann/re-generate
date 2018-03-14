module Examples.LLO where

import qualified Examples.IncreasingListOps as ILO
import Examples.LLOTypes as T

segmentize :: Lang -> [Lang]
segmentize = collect 0
  where
    collect n lx =
      let (lxn, lxrest) = splitWhile (\xs -> T.length xs == n) lx in 
      lxn : collect (n+1) lxrest

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p [] = ([], [])
splitWhile p xs@(x:xs')
  | p x = let (takes, drops) = splitWhile p xs' in (x:takes, drops)
  | otherwise = ([], xs)

concatenate :: Lang -> Lang -> Lang
concatenate lx ly = collect 0
  where
    xsegs = segmentize lx
    ysegs = segmentize ly
    collect n =
      (foldr ILO.union [] $ map (combine n) [0 .. n]) ++ collect (n+1)
    combine n i =
      [T.append x y | x <- xsegs !! i, y <- ysegs !! (n - i)]
      -- concatMap (\x -> map (T.append x) (ysegs !! (n - i))) (xsegs !! i)

star :: Lang -> Lang
star lx = concat rsegs
  where
    xsegs = segmentize lx
    rsegs = [T.empty] : collect 1
    collect n =
      (foldr ILO.union [] $ map (combine n) [1 .. n]) : collect (n + 1)
    combine n i =
      [T.append x y | x <- xsegs !! i, y <- rsegs !! (n - i)]
      -- concatMap (\xs -> map (T.append xs) (rsegs !! (n - i))) (xsegs !! i)

complement :: Alphabet -> Lang -> Lang
complement sigma lx = ILO.difference (concat lsigmastar) lx
  where
    lsigmastar = [T.empty] : map extend lsigmastar
    extend lsigmai = [T.cons a w | a <- sigma, w <- lsigmai]
