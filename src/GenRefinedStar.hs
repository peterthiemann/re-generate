module GenRefinedStar where

import GRegexp hiding (Lang)
import GenRefined.Shared
import Types (Sigma)
import qualified OrderedLists as OL

import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T


star :: Segments -> Segments
star Empty = Cons (Univ [T.empty]) Empty
star (Full ls) = Full ls
star (Cons _ xsegs) = rsegs
  where
    rsegs = Cons (Univ [T.empty]) $ collect xsegs rsegs Map.empty Map.empty [] [] 1
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
generateSegs :: Sigma -> GRE Char -> Segments
generateSegs sigma r = gen r
  where
    gen Zero = Empty
    gen One  = Cons (Data [T.empty]) Empty
    gen (Atom t) = Cons Null $ Cons (Data [T.singleton t]) Empty
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = unionSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = star (gen r)

generate :: Sigma -> GRE Char -> [T.Text]
generate sigma = flattenSegs . generateSegs sigma

generate' :: Sigma -> GRE Char -> [[T.Text]]
generate' sigma = flattenSegs' . generateSegs sigma
