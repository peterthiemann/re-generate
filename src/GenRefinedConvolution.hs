module GenRefinedConvolution (generate, generate') where

import GRegexp hiding (Lang)
import GenRefined.Shared hiding (concatenate)
import Types (Alphabet)

import Control.Applicative
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

updateMax :: Int -> Segments -> Maybe Int -> Maybe Int -> (Lang, Segments, Maybe Int, Maybe Int)
updateMax n _ mm@(Just _) mf = (Null, Empty, mm, mf)
updateMax n Empty Nothing mf = (Null, Empty, Just n, mf)
updateMax n (Cons xs xss) Nothing Nothing = (xs, xss, Nothing, Nothing)
updateMax n (Full (xs:xss)) Nothing Nothing = (xs, Full xss, Nothing, Just n)
updateMax n (Full (xs:xss)) Nothing mf@(Just _) = (xs, Full xss, Nothing, mf)

sigmaStarFromNth :: Alphabet -> Int -> Segments
sigmaStarFromNth sigma n = fromNthFull $ sigmaStarSegs sigma
  where
    fromNthFull (Full xss) = Full (drop n xss)

concatenate :: Alphabet -> Segments -> Segments -> Segments
concatenate sigma xsegs0 ysegs0 =
  collect xsegs0 ysegs0 Nothing Nothing Nothing Nothing [] 0
  where
    collect xsegs ysegs mmx mmy mfx mfy ryss n =
      let (xln, xsegs', mmx', mfx') = updateMax n xsegs mmx mfx
          (yln, ysegs', mmy', mfy') = updateMax n ysegs mmy mfy
          mbound = liftA2 (+) mmx mmy
          mfullbound = liftA2 (+) mfx mfy
          ryss' = yln : ryss
      in
        case (mbound, mfullbound) of
          (Just m, _) | n >= m - 1 ->
                   Empty
          (_, Just f) | n >= f ->
                   sigmaStarFromNth sigma n
          _ ->
            Cons 
            (foldr union Null $ concatWithSegments xsegs0 ryss')
            (case mmy' of
                Nothing ->
                  collect xsegs' ysegs' mmx' mmy' mfx' mfy' ryss' (n+1)
                Just _ ->
                  collect' xsegs' ysegs' mmx' mmy' mfx' mfy' (reverse (take (n+1) (segsToList xsegs0))) (n+1))

    collect' xsegs ysegs mmx mmy mfx mfy rxss n =
      let (xln, xsegs', mmx', mfx') = updateMax n xsegs mmx mfx
          (yln, ysegs', mmy', mfy') = updateMax n ysegs mmy mfy
          mbound = liftA2 (+) mmx mmy
          mfullbound = liftA2 (+) mfx mfy
          rxss' = xln : rxss
      in
        case (mbound, mfullbound) of
          (Just m, _) | n >= m - 1 ->
                   Empty
          (_, Just f) | n >= f ->
                   sigmaStarFromNth sigma n
          _ ->
            Cons 
            (foldr union Null $ concatWithSegments' rxss' ysegs0)
            (collect' xsegs' ysegs' mmx' mmy' mfx' mfy' rxss' (n+1))

concatWithSegments :: Segments -> [Lang] -> [Lang]
concatWithSegments Empty _ = []
concatWithSegments _ [] = []
concatWithSegments (Cons xl xsegs) (yl:yss) = 
  concatLang xl yl : concatWithSegments xsegs yss
concatWithSegments (Full (xl:xss)) (yl:yss) = 
  concatLang xl yl : zipWith concatLang xss yss

concatWithSegments' :: [Lang] -> Segments -> [Lang]
concatWithSegments' _ Empty = []
concatWithSegments' [] _ = []
concatWithSegments' (yl:yss) (Cons xl xsegs) = 
  concatLang yl xl : concatWithSegments' yss xsegs
concatWithSegments' (yl:yss) (Full (xl:xss)) = 
  concatLang yl xl : zipWith concatLang yss xss

segsToList :: Segments -> [Lang]
segsToList Empty = []
segsToList (Cons xl xsegs) = xl : segsToList xsegs
segsToList (Full xss) = xss

star :: Segments -> Segments
star Empty = Cons (Univ [mempty]) Empty
star (Full ls) = Full ls
star (Cons _ xsegs) = ysegs
  where
    ysegs = Cons (Univ [mempty]) $ collect ysegs []
    collect (Cons ysegi ysegs) rsegs =
      let rsegs' = ysegi : rsegs in
        Cons
        (foldr union Null $ concatWithSegments xsegs rsegs')
        (collect ysegs rsegs')

-- | generate elements of the language of the gre as a stream of segments
generateSegs :: Alphabet -> GRE Char -> Segments
generateSegs sigma r = gen r
  where
    gen Zero = Empty
    gen One  = Cons (Data [mempty]) Empty
    gen (Atom t) = Cons Null $ Cons (Data [T.singleton t]) Empty
    gen (Dot r s) = concatenate sigma (gen r) (gen s)
    gen (Or r s) = unionSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star (gen r)

generate :: Alphabet -> GRE Char -> [T.Text]
generate sigma = flattenSegs . generateSegs sigma

generate' :: Alphabet -> GRE Char -> [[T.Text]]
generate' sigma = flattenSegs' . generateSegs sigma
