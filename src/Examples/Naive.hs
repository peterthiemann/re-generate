module Examples.Naive where
import Types
import qualified Data.Text as T

union :: Lang -> Lang -> Lang
union lx ly = lx ++ ly

concatenate :: Lang -> Lang -> Lang
concatenate lx ly = [T.append wx wy | wx <- lx, wy <- ly ]

intersect :: Lang -> Lang -> Lang
intersect lx ly = [wx | wx <- lx, wx `elem` ly ]

star :: Lang -> Lang
star lx = concat lxi
  where
    lxi = [T.empty] : map (concatenate lx) lxi

complement :: Alphabet -> Lang -> Lang
complement sigma lx =
  undefined
