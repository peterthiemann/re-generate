module GenRefined where

import qualified Data.Map.Strict as Map

import qualified OrderedLists as OL
import GRegexp hiding (Lang)

data Lang t
  = Null
  | Data { lot :: [[t]]} -- shouldn't be empty
  | Univ { lot :: [[t]]} -- just in case

mkData :: [[t]] -> Lang t
mkData [] = Null
mkData xss = Data xss

union :: (Ord t) => Lang t -> Lang t -> Lang t
union Null yss = yss
union (Univ xss) yss = (Univ xss)
union xss Null = xss
union xss (Univ yss) = Univ yss
union (Data xss) (Data yss) = mkData (OL.merge xss yss)

intersect :: (Ord t) => Lang t -> Lang t -> Lang t
intersect Null yss = Null
intersect (Univ _) yss = yss
intersect xss Null = Null
intersect xss (Univ _) = xss
intersect (Data xss) (Data yss) = mkData (OL.intersect xss yss)

difference :: (Ord t) => Lang t -> Lang t -> Lang t
difference xss Null = xss
difference xss (Univ _) = Null
difference Null yss = Null
difference (Univ xss) (Data yss) = mkData (OL.difference xss yss)
difference (Data xss) (Data yss) = mkData (OL.difference xss yss)

concatLang :: Lang t -> Lang t -> Lang t
concatLang Null _ = Null
concatLang _ Null = Null
concatLang (Univ xss) (Univ yss) =
  Univ $ concatMap (\xs -> map (xs++) yss) xss
concatLang xls yls =
  Data $ concatMap (\xs -> map (xs++) (lot yls)) (lot xls)

flattenLang :: Lang t -> [[t]]
flattenLang Null = []
flattenLang (Data tss) = tss
flattenLang (Univ tss) = tss

-- segments
data Segments t
  = Empty
  | Cons (Lang t) (Segments t)
  | Full [Lang t]  -- all of which are (Univ xss)

unionSegs :: (Ord t) => Segments t -> Segments t -> Segments t
unionSegs Empty segs = segs
unionSegs (Full xls) segs = Full xls
unionSegs segs Empty = segs
unionSegs segs (Full yls) = Full yls
unionSegs (Cons xl xsegs) (Cons yl ysegs) =
  Cons (union xl yl) (unionSegs xsegs ysegs)

intersectSegs :: (Ord t) => Segments t -> Segments t -> Segments t
intersectSegs Empty segs = Empty
intersectSegs (Full _)  segs = segs
intersectSegs segs  Empty = Empty
intersectSegs segs  (Full _)  = segs
intersectSegs (Cons xl xsegs) (Cons yl ysegs) =
  Cons (intersect xl yl) (intersectSegs xsegs ysegs)

differenceSegs :: (Ord t) => Segments t -> Segments t -> Segments t
differenceSegs segs  Empty = segs
differenceSegs segs  (Full _) = segs
differenceSegs Empty segs = Empty
differenceSegs (Full (xl : xls)) (Cons yl ysegs) =
  Cons (difference xl yl) (differenceSegs (Full xls) ysegs)

sigmaStarSegs :: [t] -> Segments t
sigmaStarSegs sigma = Full (map Univ segments)
  where
    segments = [[]] : map extend segments
    extend segment = concatMap (\x -> map (x:) segment) sigma

complementSegs :: (Ord t) => [t] -> Segments t -> Segments t
complementSegs sigma = differenceSegs (sigmaStarSegs sigma)

flattenSegs :: Segments t -> [[t]]
flattenSegs Empty = []
flattenSegs (Cons lang segs) = flattenLang lang ++ flattenSegs segs
flattenSegs (Full langs) = concatMap flattenLang langs

updateMapIndexes :: Int -> Segments t -> Map.Map Int (Lang t) -> [Int] -> (Segments t, Map.Map Int (Lang t), [Int])
updateMapIndexes n segs mil nidxs =
  case segs of
    Empty ->
      (Empty, Map.insert n Null mil, nidxs)
    Cons lang segs' ->
      (segs', Map.insert n lang mil, n : nidxs)
    Full (lang: langs) ->
      (Full langs, Map.insert n lang mil, n : nidxs)


concatenate :: (Ord t) => Segments t -> Segments t -> Segments t
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


-- | generate elements of the language of the gre as a stream of segments
generateSegs :: (Ord t) => [t] -> GRE t -> Segments t
generateSegs sigma r = gen r
  where
    gen Zero = Empty
    gen One  = Cons (Data [[]]) Empty
    gen (Atom t) = Cons Null $ Cons (Data [[t]]) Empty
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = unionSegs (gen r) (gen s)
    gen (And r s) = intersectSegs (gen r) (gen s)
    gen (Not r) = complementSegs sigma (gen r)
    gen (Star r) = undefined -- star (gen r)

generate :: (Ord t) => [t] -> GRE t -> [[ t ]]
generate sigma = flattenSegs . generateSegs sigma

