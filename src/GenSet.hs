{-# LANGUAGE BangPatterns #-}
module GenSet (generate', generate) where

import GRegexp
import Types

import Control.Monad
import Data.List (sort)
import Data.Monoid
import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL

replicateToSize :: T.Text -> Int -> [T.Text]
replicateToSize x targetSize =
    map (TL.toStrict . TL.toLazyText) $
    DL.toList $ loop iterations (DL.singleton mempty) mempty
    where
      inSize = T.length x
      chunk = TL.fromText x
      iterations :: Int
      iterations = floor $ fromIntegral targetSize / fromIntegral inSize
      loop !it !out !accum
          | it <= 0 = out
          | otherwise =
                let nextStr = accum <> chunk
                in loop (it - 1) (out `DL.snoc` nextStr) nextStr

makeForLength :: [T.Text] -> Int -> GRE Char -> S.Set T.Text
makeForLength antiSet len re =
    case re of
      One -> if len == 0 then S.singleton mempty else mempty
      Atom x -> if len == 1 then S.singleton (T.singleton x) else mempty
      _ -> S.filter (\x -> T.length x == len) $ go re
    where
      go r =
          case r of
            Zero -> mempty
            One -> S.singleton mempty
            Atom x -> S.singleton (T.singleton x)
            Or r s -> go r `S.union` go s
            And r s -> go r `S.intersection` go s
            Not r ->
                let res = go r
                in S.fromAscList $ filter (\x -> not (x `S.member` res)) antiSet
            Dot r s ->
                let dotIn =
                        do x <- F.toList $ go r
                           y <- F.toList $ go s
                           pure (x, y)
                in S.fromList $
                   filter (\x -> T.length x <= len) $
                   fmap (\(lhs, rhs) -> lhs <> rhs) dotIn
            Star r ->
                let starIn =
                        F.toList $ go r
                    starOut =
                        S.fromList $
                        filter (\x -> T.length x <= len) $
                        concatMap (flip replicateToSize len) starIn
                in starOut

generate' :: Alphabet -> GRE Char -> Segments
generate' sigmaIn re =
    let sigma = sort sigmaIn
        sigmaSize = length sigma
        streamYield !i =
            fmap T.pack (DL.fromList $ replicateM i sigma) <> streamYield (i+1)
        antiSetStream :: DL.DList T.Text
        antiSetStream = streamYield 0
        takeUntil 0 = 1
        takeUntil n = takeUntil (n - 1) + sigmaSize ^ n
    in map (\i -> F.toList $ makeForLength (take (takeUntil i) $ DL.toList antiSetStream) i re) [0..]

generate :: Alphabet -> GRE Char -> Lang
generate sigma = concat . generate' sigma
