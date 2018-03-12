module LLeq where

import qualified Data.Text as T

-- | length lexicographic ordering
lleq :: (Ord t) => [t] -> [t] -> Bool
lleq xs ys =
  let lxs = length xs
      lys = length ys
  in  lxs < lys ||
      lxs == lys && xs <= ys


-- | length lexicographic ordering for text
lleqt :: T.Text -> T.Text -> Bool
lleqt xs ys =
  let lxs = T.length xs
      lys = T.length ys
  in  lxs < lys ||
      lxs == lys && xs <= ys
