module ParserCon
-- | The following functions are exported:
  ( module Control.Applicative -- ^ This exports the operators (<*, *>, <*>, <|>,...)
    --   and combintors `empty' and `many'
  , module Control.Monad
  , Parser       -- ^ the parser newtype
  , lit          -- ^ versions of old combinators that work with the newtype
  , satisfy
  , try
  , parse        -- ^ return (Just result) on success
  , parseAll     -- ^ returns the `list of successes' (just applies the underlying RawParser)
  , parseLongest -- ^ returns the longest possible parse (as ParseResult value)
  ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe

-- ^ Execution of a parser
parse :: Parser t r -> [t] -> Maybe r
parse p ts = case  parseLongest p ts of
               Match x -> Just x
               _ -> Nothing

data ParseResult t r = Match r | Partial (r, [t]) | Fail
  deriving (Show, Eq)

parseLongest :: Parser t r -> [t] -> ParseResult t r
parseLongest p ts =  maybe (tryPartial allResults) Match $  tryMatch allResults
   where allResults = parseAll p ts
         tryMatch rs = fmap fst $ listToMaybe $ dropWhile (not . null . snd) rs
         tryPartial [] = Fail
         tryPartial xs = Partial $ minimumBy (compare `on` (length . snd)) xs

parseAll :: Parser t r -> [t] -> [(r, [t])]
parseAll p = rawParser p

-- ^ RawParser, Called "Parser" in the course
type RawParser token result = [token] -> [(result, [token])]

pempty :: RawParser t r
pempty ts = []

psucceed :: r -> RawParser t r
psucceed r ts = [(r, ts)]

psatisfy :: (t -> Bool) -> RawParser t t
psatisfy p [] = []
psatisfy p (t:ts) | p t       = [(t, ts)]
                  | otherwise = []


msatisfy :: (t -> Maybe a) -> RawParser t a
msatisfy f [] = []
msatisfy f (t:ts) = case f t of
                      Nothing -> []
                      Just a  -> psucceed a ts

plit :: Eq t => t -> RawParser t t
plit t = psatisfy (== t)

palt :: RawParser t r -> RawParser t r -> RawParser t r
palt p1 p2 = \ts -> p1 ts ++ p2 ts

pseq :: RawParser t (s -> r) -> RawParser t s -> RawParser t r
pseq p1 p2 ts = [ (f s, ts'') | (f, ts') <- p1 ts, (s, ts'') <- p2 ts' ]

pmap :: (s -> r) -> RawParser t s -> RawParser t r
pmap f p ts = [ (f s, ts') | (s, ts') <- p ts]



-- ^ Modifications and instances, analogous to Parser' in the course
newtype Parser token result = P (RawParser token result)

rawParser :: Parser t r -> RawParser t r
rawParser (P p) = p

instance Functor (Parser t) where
  fmap f = P . pmap f . rawParser

instance Applicative (Parser t) where
  pure v = P $ psucceed v
  (P p1) <*> (P p2) = P (p1 `pseq` p2)

instance Alternative (Parser t) where
  empty = P pempty
  P p1 <|> P p2 = P (p1 `palt` p2)

instance Monad (Parser t) where
  return = pure
  (>>=) (P p) f = P $ \ts ->
    concatMap (\(r, rest) -> rawParser (f r) rest) $ p ts

instance MonadPlus (Parser t) where
  mzero = empty
  mplus = (<|>)

lit x = P $ plit x
satisfy p = P $ psatisfy p
try p = P $ msatisfy p
