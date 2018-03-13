{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module RegexTestLib
    (Basics(..), Features(..), Feature(..), withFeatures)
        where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Applicative (liftA2)
import Test.QuickCheck
import GRegexp as G (GRE(..))
import qualified GenSegments (generate)

{- | Using the following types, you should describe how to build
   your regular expression types. The function `withFeatures` will then use
   this description to generate a set of test cases using quickCheck.
   In each types, the type variable "a" represent your regex type.
 -}

-- | The minimal constructors required by the Core project.
--
data Basics a = Basics
    { atom :: Char -> a
    -- | a constructor for the language taking exactly this Char
    , seq :: a -> a -> a
    -- | based on two regexes, construct the regex accepting the concatenation
    , alt :: a -> a -> a
    -- | based on two regexes, construct the regex accepting the union of the elments
    , star :: a -> a
    -- | based on a regex, accept it for [0;\infty] many times.
    , empty :: a
    -- | Recognize the empty string
    , null :: a
    -- | Recognize nothing
    }


-- | Various potential featured offered by your regular expression engine.
--
type Features a = [Feature a]
data Feature a
  = Parsing
      { print :: a -> String
      -- | pring a regular expression
      , parse :: String -> Maybe a
      -- | Parse the string as a regular expression
      -- return a Void-type if it's not a valid one.
      } -- | parsing and printing are supposed to be Idempotent.
  | Set ([Char] -> a)
  -- | Set of characters, `[abcd]`
  | Any a
  -- | Any character, `.`
  | Rep (Int -> (Maybe Int) -> a -> a)
  -- | Optionally bounded repetition such as `a{3,4}` or `.{4,}`
  | Many (a -> a)
  -- | At least one repetition, `a+`.
  | Simplify (a -> a)
  -- | Simplifycation of regular expressions.
  | Union (a -> a -> a)
  -- | And on regular expression
  | Match (a -> T.Text -> Bool)
  -- | Matching function


-- | Generate a set of test cases depending on the provided features.
--
withFeatures :: (Eq a, Show a) => Basics a -> Features a -> IO ()
withFeatures b fa = do
  mapM_ quickCheck $ map (\p -> p b fa) feature_property



----------- Everything below this line is not exported, DO NOT USE ------------

alphabet = "abcdef"

-- | Generate synchronized Regex and GRegex.
--
genRe :: (Eq a, Show a) => Basics a -> Features a -> Gen (a, GRE Char)
genRe b@(Basics atom seq alt star empty null) f = do
  size <- getSize
  if size <= 0
  then genBaseCase
  else resize (size -1) $ frequency $ genWithBasics ++ (f >>= genWithFeature)
    where
      a = scale (\i -> i - 1) $ genRe b f
      char = elements alphabet
      charset = listOf1 $ elements alphabet

      genBaseCase = char >>= \c -> return (atom c, Atom c)

      genWithBasics = [
         (1, return (null, Zero)), 
         (1, return (empty, One)),
         (10, genBaseCase),
         (4, app2 seq Dot),
         (4, app2 alt Or),
         (2, app1 star Star)
       ]
      -- genWithFeature (Set set) =
      --     [(8,  charset >>= \cs -> return (set cs, GSet cs))]
      -- genWithFeature (Any any) = [(1, return (any, GSet alphabet))]
      genWithFeature (Many many) = [(2, app1 many (\x -> Dot x (Star x)))]
      genWithFeature (Union and) =
          [ (2, app2 and And) ]
      -- genWithFeature (Rep rep) =
      --     [ (1, do { i <- choose (0, 2); app1 (rep i Nothing) (GRep i Nothing)})
      --     , (1, do { i <- choose (0, 2)
      --              ; j <- choose (max 1 i, i+2)
      --              ; app1 (rep i (Just j)) (GRep i (Just j)) })
      --     ]
      genWithFeature _ = []

      app1 f g = do { (re, gre) <- a ; return (f re, g gre) }
      app2 f g =
          do { (r1, gr1) <- a ; (r2, gr2) <- a ; return (f r1 r2, g gr1 gr2) }


-- | helper-function for inhabitants, returning the set of possible inhabitants
-- based on this regex. (The Regex should match those.)
--
reToList :: GRE Char -> [T.Text]
reToList = GenSegments.generate alphabet


-- | Generator for the inhabitants of this Regex
--
inhabitants :: GRE Char -> Gen T.Text
inhabitants re =
  let ss = reToList re in
  if List.null ss
  then discard
  else elements ss


-- | Returns a feature or discards the test
--
getFeature :: (p -> Bool) -> [p] -> p
getFeature p f =
  case List.find p f of
    Just m -> m
    Nothing -> discard



----------- Everything below this line are the actual testcases  ------------


prop_matching b f =
    counterexample
    "This regular expression should match theses strings:"
    $ withMaxSuccess 10000 $ forAll gen test
    where
      (Match match) = getFeature (\case { Match _ -> True; _ -> False}) f
      gen = do
        (re, gre) <- resize 4 $ genRe b f
        l <- List.nub <$> (vectorOf 20 $ inhabitants gre)
        return (re, l)
      test (re, l) = within (10000 * List.length l) $ all (match re) l

prop_simplify b f =
    counterexample
    "The second regular expression is the simplification of the first one, but they are not equivalent on this string:"
    $ withMaxSuccess 5000 $ forAll gen test
    where
      (Simplify simpl) = getFeature (\case { Simplify _ -> True; _ -> False}) f
      (Match match) = getFeature (\case { Match _ -> True; _ -> False}) f
      gen = do
        (re, _) <- resize 10 $ genRe b f
        let re2 = simpl re
        return (re, re2)
      test (re, re2) =
          forAll (resize 60 $ listOf $ elements alphabet) $
                     (\s -> match re s == match re2 s) . T.pack

prop_any b f =
    counterexample "The following character should be matched by `any`" $
    all (match any) allAny
  where
  (Match match) = getFeature (\case { Match _ -> True; _ -> False}) f
  (Any   any)   = getFeature (\case { Any _ -> True; _ -> False}) f
  allAny = map T.singleton $ enumFromTo ' ' '~'



-- | List of testcases still requiring the basic functionality as well as
-- features.
--
feature_property :: (Eq a, Show a) => [Basics a -> Features a -> Property]
feature_property =
  [ prop_matching
  , prop_any
  , prop_simplify
  ]
