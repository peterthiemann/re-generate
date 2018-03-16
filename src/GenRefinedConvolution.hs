module GenRefinedConvolution where

import GRegexp hiding (Lang)
import GenRefined.Shared hiding (concatenate)
import Types (Alphabet)
import qualified OrderedLists as OL

import Control.Applicative
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

updateMax :: Segments -> Maybe Int -> Int -> (Lang, Segments, Maybe Int)
updateMax _ mm@(Just _) n = (Null, Empty, mm)
updateMax Empty Nothing n = (Null, Empty, Just n)
updateMax (Cons xs xss) Nothing n = (xs, xss, Nothing)
updateMax (Full (xs:xss)) Nothing n = (xs, Full xss, Nothing)

concatenate :: Segments -> Segments -> Segments
concatenate xsegs0 ysegs =
  collect xsegs0 ysegs Nothing Nothing [] 0
  where
    collect xsegs ysegs mmx mmy ryss n =
      let (xln, xsegs', mmx') =
            updateMax xsegs mmx n
          (yln, ysegs', mmy') =
            updateMax ysegs mmy n
          mbound = liftA2 (+) mmx mmy
          ryss' = yln : ryss
      in
        case mbound of
          Just m | n >= m - 1 ->
                   Empty
          _ ->
            Cons 
            (foldr union Null $ concatWithSegments xsegs0 ryss')
            (collect xsegs' ysegs' mmx' mmy' ryss' (n+1))

concatWithSegments :: Segments -> [Lang] -> [Lang]
concatWithSegments Empty _ = []
concatWithSegments _ [] = []
concatWithSegments (Cons xl xsegs) (yl:yss) = 
  concatLang xl yl : concatWithSegments xsegs yss
concatWithSegments (Full (xl:xss)) (yl:yss) = 
  concatLang xl yl : zipWith concatLang xss yss

star :: Segments -> Segments
star Empty = Cons (Univ [mempty]) Empty
star (Full ls) = Full ls
star (Cons _ xsegs) = rsegs
  where
    rsegs = Cons (Univ [mempty]) $ collect xsegs rsegs Map.empty Map.empty [] [] 1
    collect xsegs rsegs xmap rmap xneidxs rneidxs n =
      let (xsegs', xmap', xneidxs') =
            updateMapIndexes n xsegs xmap xneidxs
          (rsegs', rmap', rneidxs') =
            updateMapIndexes (n-1) rsegs rmap rneidxs
          usefulxidxs = filter (\i -> (n-i) `elem` rneidxs') xneidxs'
          combine i = concatLang (xmap' Map.! i) (rmap' Map.! (n-i))
      in
        Cons (foldr union Null $ map combine usefulxidxs)
             (collect xsegs' rsegs' xmap' rmap' xneidxs' rneidxs' (n+1))

-- | generate elements of the language of the gre as a stream of segments
generateSegs :: Alphabet -> GRE Char -> Segments
generateSegs sigma r = gen r
  where
    gen Zero = Empty
    gen One  = Cons (Data [mempty]) Empty
    gen (Atom t) = Cons Null $ Cons (Data [T.singleton t]) Empty
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = unionSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star (gen r)

generate :: Alphabet -> GRE Char -> [T.Text]
generate sigma = flattenSegs . generateSegs sigma

generate' :: Alphabet -> GRE Char -> [[T.Text]]
generate' sigma = flattenSegs' . generateSegs sigma
