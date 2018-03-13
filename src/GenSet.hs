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

replicateToSize :: T.Text -> Int -> [T.Text]
replicateToSize x targetSize =
    DL.toList $ loop (DL.singleton mempty) mempty
    where
      loop !out !accum =
          let nextStr = accum <> x
          in if T.length nextStr <= targetSize
             then loop (out `DL.snoc` nextStr) nextStr
             else out

makeForLength :: S.Set T.Text -> Int -> GRE Char -> S.Set T.Text
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
            Not r -> antiSet `S.difference` go r
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

generate' :: Sigma -> GRE Char -> Segments
generate' sigmaIn re =
    let sigma = sort sigmaIn
        makeAntiSet prev i =
            prev <> S.fromAscList (map T.pack $ replicateM i sigma)
        streamYield !prev !i =
            let prev' = makeAntiSet prev i
            in prev' : streamYield prev' (i+1)
        antiSetStream :: [S.Set T.Text]
        antiSetStream = streamYield (S.singleton mempty) 0
    in map (\i -> F.toList $ makeForLength (antiSetStream !! i) i re) [0..]

generate :: Sigma -> GRE Char -> Lang
generate sigma = concat . generate' sigma
