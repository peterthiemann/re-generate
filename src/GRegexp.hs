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

