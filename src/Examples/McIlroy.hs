module Examples.McIlroy where
import qualified Data.Text as T

type Alphabet = [Char]
type Lang = [T.Text]

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
star lx@(x:xt)
  | x == T.empty =
    star xt
  | otherwise =
    T.empty : concatenate lx (star lx)


intersect :: Lang -> Lang -> Lang
intersect xs@(x:xs') ys@(y:ys') =
  case compare x y of
    EQ -> x : intersect xs' ys'
    LT -> intersect xs' ys
    GT -> intersect xs ys'
intersect xs ys = []

difference :: Lang -> Lang -> Lang
difference xs@(x:xs') ys@(y:ys') =
  case compare x y of
    EQ -> difference xs' ys'
    LT -> x : difference xs' ys
    GT -> difference xs ys'
difference xs ys = xs

complement :: Alphabet -> Lang -> Lang
complement sigma lx = difference lsigmastar lx
  where
    lsigmastar = star (map T.singleton sigma)

