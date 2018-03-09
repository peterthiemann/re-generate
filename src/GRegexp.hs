module GRegexp where

import Data.List

-- | generalized regular expressions - with intersection and negation
data GRE t
    = Zero
    | One
    | Atom t
    | Dot (GRE t) (GRE t)
    | Or (GRE t) (GRE t)
    | And (GRE t) (GRE t)
    | Not (GRE t)
    | Star (GRE t)
    deriving (Eq, Ord, Show)

-- | extract the list of atoms from regular expression
atoms :: (Ord t) => GRE t -> [t]
atoms Zero = []
atoms One = []
atoms (Atom t) = [t]
atoms (Dot r s) = union (atoms r) (atoms s)
atoms (Or r s) = union (atoms r) (atoms s)
atoms (And r s) = union (atoms r) (atoms s)
atoms (Not r) = atoms r
atoms (Star r) = atoms r

type Lang t = [[t]]

-- | naive implementation
naive ::(Eq t) => GRE t -> Lang t
naive Zero = []
naive One = [[]]
naive (Atom t) = [[t]]
naive (Dot r s) = [ v ++ w | v <- naive r, w <- naive s]
naive (Or r s) = union (naive r) (naive s)
naive (And r s) = intersect (naive r) (naive s)
naive (Not r) = undefined
naive (Star r) = [] : [ v ++ w | v <- naive r, w <- naive (Star r) ]

-- drawbacks
-- * no effective element test
-- * how to implement not?
-- * easy to get repetitions
-- * efficiency?

