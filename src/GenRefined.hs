module GenRefined where

import GRegexp hiding (Lang)
import GenRefined.Shared
import Types (Alphabet)
import qualified OrderedLists as OL

import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

star :: Segments -> Segments
star Empty = Cons (Univ [mempty]) Empty
star (Full ls) = Full ls
star (Cons _ xsegs) = Cons (Univ [mempty]) $ collect xsegs Map.empty [] 1
  where
    collect xsegs xmap xneidxs n =
      let (xsegs', xmap', xneidxs') =
            updateMapIndexes n xsegs xmap xneidxs
      in
        Cons (foldr union Null $
              map (langFromPartition xmap') (restrictedPartitions' xneidxs' n))
             (collect xsegs' xmap' xneidxs' (n+1))

langFromPartition :: Map.Map Int Lang -> [Int] -> Lang
langFromPartition msegs [i] = msegs Map.! i
langFromPartition msegs (i:is) = concatLang (msegs Map.! i) (langFromPartition msegs is)

-- | pn = restrictedPartitions ns n
-- xs \in pn => sum xs = n, xi \in xs => xi \in ns /\ xi > 0
-- no repetitions
-- ns is sorted decreasingly
restrictedPartitions' :: [Int] -> Int -> [[Int]]
restrictedPartitions' [] n = [[]]
restrictedPartitions' ns n
  | n == 0 = [[]]
  | otherwise = let ns' = dropWhile (>n) ns in concatMap (\i -> map (i:) (restrictedPartitions' ns' (n - i))) ns'

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
