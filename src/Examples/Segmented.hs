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
            Just m | n >= m - 1 ->
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

  difference = difference'

difference' [] ly = []
difference' lx [] = lx
difference' (sx:lx) (sy:ly) = ILO.difference sx sy : difference' lx ly

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

-- | the convolution approach
concatenate'' :: SegLang -> SegLang -> SegLang
concatenate'' lx ly = collect ly []
  where
    collect (ysegn:ysegs) rly =
      let rly' = ysegn : rly in
      (foldr ILO.union [] $ zipWith (liftA2 T.append) lx rly') :
      collect ysegs rly'

-- | convolution and finiteness
concatenate''' :: SegLang -> SegLang -> SegLang
concatenate''' lx ly =
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
              (foldr ILO.union [] $ zipWith (liftA2 T.append) lx ryss') :
              collect xss' yss' mmx' mmy' ryss' (n+1)
      updateMax _ mm@(Just _) n = ([], [], mm)
      updateMax [] Nothing n = ([], [], Just n)
      updateMax (xs:xss) Nothing n = (xs, xss, Nothing)

