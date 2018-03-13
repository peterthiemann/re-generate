module GenRefined.Shared where

import GRegexp hiding (Lang)
import Types (Alphabet)
import qualified OrderedLists as OL

import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Lang
  = Null
  | Data { lot :: [T.Text]} -- shouldn't be empty
  | Univ { lot :: [T.Text]} -- just in case

mkData :: [T.Text] -> Lang
mkData [] = Null
mkData xss = Data xss

union :: Lang -> Lang -> Lang
union Null yss = yss
union (Univ xss) yss = (Univ xss)
union xss Null = xss
union xss (Univ yss) = Univ yss
union (Data xss) (Data yss) = mkData (OL.merge xss yss)

intersect :: Lang -> Lang -> Lang
intersect Null yss = Null
intersect (Univ _) yss = yss
intersect xss Null = Null
intersect xss (Univ _) = xss
intersect (Data xss) (Data yss) = mkData (OL.intersect xss yss)

difference :: Lang -> Lang -> Lang
difference xss Null = xss
difference xss (Univ _) = Null
difference Null yss = Null
difference (Univ xss) (Data yss) = mkData (OL.difference xss yss)
difference (Data xss) (Data yss) = mkData (OL.difference xss yss)

concatLang :: Lang -> Lang -> Lang
concatLang Null _ = Null
concatLang _ Null = Null
concatLang (Univ xss) (Univ yss) =
  Univ $ concatMap (\xs -> map (xs<>) yss) xss
concatLang xls yls =
  Data $ concatMap (\xs -> map (xs<>) (lot yls)) (lot xls)

flattenLang :: Lang -> [T.Text]
flattenLang Null = []
flattenLang (Data tss) = tss
flattenLang (Univ tss) = tss

-- segments
data Segments
  = Empty
  | Cons Lang Segments
  | Full [Lang]  -- all of which are (Univ xss)

unionSegs :: Segments -> Segments -> Segments
unionSegs Empty segs = segs
unionSegs (Full xls) segs = Full xls
unionSegs segs Empty = segs
unionSegs segs (Full yls) = Full yls
unionSegs (Cons xl xsegs) (Cons yl ysegs) =
  Cons (union xl yl) (unionSegs xsegs ysegs)

intersectSegs :: Segments -> Segments -> Segments
intersectSegs Empty segs = Empty
intersectSegs (Full _)  segs = segs
intersectSegs segs  Empty = Empty
intersectSegs segs  (Full _)  = segs
intersectSegs (Cons xl xsegs) (Cons yl ysegs) =
  Cons (intersect xl yl) (intersectSegs xsegs ysegs)

differenceSegs :: Segments -> Segments -> Segments
differenceSegs segs  Empty = segs
differenceSegs segs  (Full _) = segs
differenceSegs Empty segs = Empty
differenceSegs (Full (xl : xls)) (Cons yl ysegs) =
  Cons (difference xl yl) (differenceSegs (Full xls) ysegs)

sigmaStarSegs :: Alphabet -> Segments
sigmaStarSegs sigma = Full (map Univ segments)
  where
    segments = [mempty] : map extend segments
    extend segment = concatMap (\x -> map (T.singleton x<>) segment) sigma

complementSegs :: Alphabet -> Segments -> Segments
complementSegs sigma = differenceSegs (sigmaStarSegs sigma)

flattenSegs :: Segments -> [T.Text]
flattenSegs Empty = []
flattenSegs (Cons lang segs) = flattenLang lang <> flattenSegs segs
flattenSegs (Full langs) = concatMap flattenLang langs

flattenSegs' :: Segments -> [[T.Text]]
flattenSegs' Empty = []
flattenSegs' (Cons lang segs) = flattenLang lang : flattenSegs' segs
flattenSegs' (Full langs) = map flattenLang langs

updateMapIndexes :: Int -> Segments -> Map.Map Int (Lang) -> [Int] -> (Segments, Map.Map Int (Lang), [Int])
updateMapIndexes n segs mil nidxs =
  case segs of
    Empty ->
      (Empty, Map.insert n Null mil, nidxs)
    Cons lang segs' ->
      (segs', Map.insert n lang mil, n : nidxs)
    Full (lang: langs) ->
      (Full langs, Map.insert n lang mil, n : nidxs)


concatenate :: Segments -> Segments -> Segments
concatenate xsegs ysegs =
  collect xsegs ysegs Map.empty Map.empty [] [] 0
  where
    collect xsegs ysegs xmap ymap xneidxs yneidxs n =
      let (xsegs', xmap', xneidxs') =
            updateMapIndexes n xsegs xmap xneidxs
          (ysegs', ymap', yneidxs') =
            updateMapIndexes n ysegs ymap yneidxs
          combine i = concatLang (xmap' Map.! i) (ymap' Map.! (n-i))
          usefulxidxs = filter (\i -> (n-i) `elem` yneidxs') xneidxs'
      in
        Cons (foldr union Null $ map combine usefulxidxs)
             (collect xsegs' ysegs' xmap' ymap' xneidxs' yneidxs' (n+1))
