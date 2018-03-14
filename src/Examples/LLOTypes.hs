module Examples.LLOTypes where

import qualified Data.Text as T

type Alphabet = [Char]
type Lang = [LLOText]

newtype LLOText =
  LLOText T.Text
  deriving (Eq)

instance Ord LLOText where
  compare (LLOText u) (LLOText v) =
    case compare (T.length u) (T.length v) of
      EQ -> compare u v
      LT -> LT
      GT -> GT

length :: LLOText -> Int
length (LLOText x) = T.length x

append :: LLOText -> LLOText -> LLOText
append (LLOText x) (LLOText y) = LLOText (T.append x y)

empty :: LLOText
empty = LLOText T.empty

cons :: Char -> LLOText -> LLOText
cons c (LLOText x) = LLOText (T.cons c x)
