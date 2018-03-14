{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Examples.Segmented where

import qualified Data.Text as T
import qualified Examples.IncreasingListOps as ILO
import GREImpl

instance GREImpl [[T.Text]] where
  type Sym [[T.Text]] = Char
  type Wrd [[T.Text]] = T.Text

  toList = concat

  zero = []
  one = [[T.empty]]
  atom t = [[], [T.singleton t]]

  union [] ly = ly
  union lx [] = lx
  union (sx:lx) (sy:ly) = ILO.union sx sy : union lx ly

  intersect [] ly = []
  intersect lx [] = []
  intersect (sx:lx) (sy:ly) = ILO.intersect sx sy : intersect lx ly
  
  concatenate lx ly = collect 0
    where
      collect n =
        (foldr ILO.union [] $ map (combine n) [0 .. n]) : collect (n+1)
      combine n i =
        [T.append x y | x <- lx !!! i, y <- ly !!! (n - i)]

  star lx = lr
    where
      lr = [T.empty] : collect 1
      collect n =
        (foldr ILO.union [] $ map (combine n) [1 .. n]) : collect (n + 1)
      combine n i =
        [T.append x y | x <- lx !!! i, y <- lr !!! (n - i)]

  complement sigma lx = difference lsigmastar lx
    where
      lsigmastar = [T.empty] : map extend lsigmastar
      extend lsigmai = [T.cons a w | a <- sigma, w <- lsigmai]

difference [] ly = []
difference lx [] = lx
difference (sx:lx) (sy:ly) = ILO.difference sx sy : difference lx ly


-- | list index beyond the end of the list
(!!!) :: [[a]] -> Int -> [a]
[] !!! n = []
(xs:xss) !!! 0 = xs
(xs:xss) !!! n = xss !!! (n - 1)
