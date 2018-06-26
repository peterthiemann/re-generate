{-# LANGUAGE OverloadedStrings #-}
module GenMcIlroy where

import GRegexp

-- import Partitions
import Types

import Data.List hiding (intersect, union)
import Data.Maybe
import Data.Monoid
-- import qualified Data.List (intersect)
import qualified Data.Text as T

llocompare :: T.Text -> T.Text -> Ordering
llocompare u v =
  case compare (T.length u) (T.length v) of
    EQ -> compare u v
    LT -> LT
    GT -> GT

union :: Lang -> Lang -> Lang
union xs@(x:xs') ys@(y:ys') =
  case llocompare x y of
    EQ -> x : union xs' ys'
    LT -> x : union xs' ys
    GT -> y : union xs ys'
union xs ys = xs ++ ys

concatenate :: Lang -> Lang -> Lang
concatenate [] ly = []
concatenate lx [] = []
concatenate (x:xt) ly@(y:yt) =
  T.append x y : union (concatenate [x] yt) 
                       (concatenate xt ly)

star :: Lang -> Lang
star [] = [T.empty]
star lx@(x:xt) = if x == T.empty
  then star xt
  else T.empty : concatenate lx (star lx)

intersect :: Lang -> Lang -> Lang
intersect xs@(x:xs') ys@(y:ys') =
  case llocompare x y of
    EQ -> x : intersect xs' ys'
    LT -> intersect xs' ys
    GT -> intersect xs ys'
intersect xs ys = []

difference :: Lang -> Lang -> Lang
difference xs@(x:xs') ys@(y:ys') =
  case llocompare x y of
    EQ -> difference xs' ys'
    LT -> x : difference xs' ys
    GT -> difference xs ys'
difference xs ys = xs ++ ys

complement :: Alphabet -> Lang -> Lang
complement sigma lx = difference lsigmastar lx
  where
    lsigmastar = star (map T.singleton sigma)

-- | generate elements of the language of the gre as an ll-ascending stream
generate :: Alphabet -> GRE Char -> Lang
generate sigma r = gen r
  where
    gen Zero = []
    gen One  = [mempty]
    gen (Atom t) = [T.singleton t]
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = union (gen r) (gen s)
    gen (And r s) = intersect (gen r) (gen s)
    gen (Not r) = complement sigma (gen r)
    gen (Star r) = star (gen r)


-- | generate elements of the language of the gre as an ll-ascending stream; the final thing
generate' :: Alphabet -> GRE Char -> [[T.Text]]
generate' sigma r = segmentize (generate sigma r)

-- API impedance

-- | collect elements of the same length; always returns an infinite list
-- each same-length segment is sorted lexicographically
segmentize :: Lang -> [[T.Text]]
segmentize = collect 0
  where
    collect n xss =
        let (takes, drops) =
                splitWhile (\xs -> T.length xs == n) xss
        in takes : collect (n+1) drops

-- | combination of takeWhile and dropWhile
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p [] = ([], [])
splitWhile p xs@(x:xs')
  | p x = let (takes, drops) = splitWhile p xs' in (x:takes, drops)
  | otherwise = ([], xs)
