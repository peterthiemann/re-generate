{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Examples.Segmented where

import Control.Applicative

import qualified Data.Text as T
import qualified Examples.IncreasingListOps as ILO
import GREImpl

type SegLang = [[T.Text]]

instance GREImpl SegLang where
  type Sym SegLang = Char
  type Wrd SegLang = T.Text

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
  
  concatenate lx ly =
    collect lx ly Nothing Nothing 0
    where
      collect xss yss mmx mmy n =
        let (xss', mmx') = updateMax xss mmx n
            (yss', mmy') = updateMax yss mmy n
            mbound = liftA2 (+) mmx mmy
        in
          case mbound of
            Just m | n > m ->
                     []
            _ ->
              (foldr ILO.union [] $ map (combine n) [0 .. n]) :
              collect xss' yss' mmx' mmy' (n+1)
      combine n i =
        [T.append x y | x <- lx !!! i, y <- ly !!! (n - i)]
      updateMax _ mm@(Just _) n = ([], mm)
      updateMax [] Nothing n = ([], Just n)
      updateMax (_:xss) Nothing n = (xss, Nothing)

  star [] = one
  star [_] = one
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

concatenate' :: SegLang -> SegLang -> SegLang
concatenate' lx ly = collect 0
  where
    collect n =
      (foldr ILO.union [] $ map (combine n) [0 .. n]) : collect (n+1)
    combine n i =
      [T.append x y | x <- lx !!! i, y <- ly !!! (n - i)]

-- | list index beyond the end of the list
(!!!) :: [[a]] -> Int -> [a]
[] !!! n = []
(xs:xss) !!! 0 = xs
(xs:xss) !!! n = xss !!! (n - 1)
