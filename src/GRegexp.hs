{-# LANGUAGE DeriveGeneric #-}
module GRegexp (GRE(..)) where

import Types

import Control.DeepSeq
import qualified Data.List as L
import GHC.Generics
import qualified Data.Text as T

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
    deriving (Eq, Ord, Show, Generic)

instance NFData t => NFData (GRE t)

-- | extract the list of atoms from regular expression
atoms :: (Ord t) => GRE t -> [t]
atoms Zero = []
atoms One = []
atoms (Atom t) = [t]
atoms (Dot r s) = L.union (atoms r) (atoms s)
atoms (Or r s) = L.union (atoms r) (atoms s)
atoms (And r s) = L.union (atoms r) (atoms s)
atoms (Not r) = atoms r
atoms (Star r) = atoms r

