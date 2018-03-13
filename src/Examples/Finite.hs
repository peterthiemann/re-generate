module Examples.Finite where
import Types
import Data.List as L
import qualified Data.Text as T

limit :: Int
limit = 1024

union :: Lang -> Lang -> Lang
union lx ly = L.union lx ly

concatenate :: Lang -> Lang -> Lang
concatenate lx ly = L.nub [T.append wx wy | wx <- lx, wy <- ly ]

intersect :: Lang -> Lang -> Lang
intersect lx ly = [wx | wx <- lx, wx `elem` ly ]

star :: Lang -> Lang
star lx = take limit $ removeDuplicates $ concat lxs
  where
    lxs = [T.empty] : map (concatenate lx1) lxs
    lx1 = L.delete T.empty lx
    removeDuplicates [] = []
    removeDuplicates (w:ws) = w : removeDuplicates (filter (/=w) ws)

complement :: Alphabet -> Lang -> Lang
complement sigma lx = take limit (concat lsigmastar L.\\ lx) 
  where
    lsigmastar =
      [T.empty] : 
      map (\lsigmai -> concatMap (\la -> concatenate la lsigmai) lsigma1) lsigmastar
    lsigma1 = map (return . T.singleton) sigma
