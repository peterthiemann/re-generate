{-# LANGUAGE TypeFamilies #-}
module GREImpl where
import Types
import GRegexp

-- | interface for implementing generators
class GREImpl lang where
  type Sym lang
  type Wrd lang

  toList :: lang -> [Wrd lang]

  zero :: lang
  one  :: lang
  atom :: Sym lang -> lang
  union :: lang -> lang -> lang
  concatenate :: lang -> lang -> lang
  star :: lang -> lang
  intersect :: lang -> lang -> lang
  difference :: lang -> lang -> lang


generate :: GREImpl lang => [Sym lang] -> GRE (Sym lang) -> lang
generate sigma = gen where
  gen Zero = zero
  gen One = one
  gen (Atom t) = atom t
  gen (Or r s) = union (gen r) (gen s)
  gen (Dot r s) = concatenate (gen r) (gen s)
  gen (Star r) = star (gen r)
  gen (And r s) = intersect (gen r) (gen s)
  gen (Not r) = difference sigmastar (gen r)
  sigmastar = star (foldr union zero $ map atom sigma)
