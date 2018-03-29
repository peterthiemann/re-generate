module Main where

import Criterion.Main

import Data.List (sort)

import GRegexp
import Generator
import RegexParser
import Types (Alphabet)

regexEnv :: (GRE Char -> GRE Char) -> String -> GRE Char
regexEnv f input =
    case parseRe input of
      Nothing -> error "Invalid regex"
      Just ok -> f ok

matchGroup :: Maybe Int -> Maybe Int -> String -> (GRE Char -> GRE Char) -> String -> Benchmark
matchGroup maxLength maxWords complementAlphabet f input =
     env (pure $ regexEnv f input) $ \ ~re ->
     bgroup (infoString re) $
     flip map [minBound..maxBound] $ \gen ->
     let config =
             GeneratorConfig
             { gc_backend = gen
             , gc_maxLength = maxLength
             , gc_complementAlphabet = sort complementAlphabet
             }
         benchmark =
             case maxWords of
               Nothing -> nf (concat . runGenerator config) re
               Just x -> nf (take x . concat . runGenerator config) re
     in bench ("gen=" ++ show gen) benchmark
     where
       infoString re =
           "generation for " ++ pretty re ++ " with maxLen="
           ++ show maxLength ++ " and complement alpha=" ++ complementAlphabet
           ++ " and maxWords=" ++ show maxWords

lengthBounds :: [Int]
lengthBounds = [8, 12, 16, 20]

matchGroups :: Alphabet -> String -> [Benchmark]
matchGroups sigma reString = do
  maxLength <- lengthBounds
  transform <- [id, Not]
  return $ matchGroup (Just maxLength) Nothing sigma transform reString

main :: IO ()
main =
    defaultMain $ concat
    [ matchGroups "ab" "a*"
    , matchGroups "ab" "a*b"
    , matchGroups "ab" "ab*"
    , matchGroups "ab" "ba*b"
    , matchGroups "ab" "a*b*"
    , matchGroups "ab" "~(a*)|a*"
    , matchGroups "ab" "~(a*)&a*"
    , matchGroups "ab" "(b*ab*a)*b*"
    , matchGroups "ab" "((b*ab*a)*b*)&((a*ba*b)*a*)"
    , matchGroups "ab" "((b*ab*a)*b*)&~((a*ba*b)*a*)"
    , matchGroups "ab" "(b*ab*ab*a)*b*"
    , matchGroups "ab" "((b*ab*ab*a)*b*)&((a*ba*ba*b)*a*)"
    , matchGroups "ab" "((b*ab*ab*a)*b*)&~((a*ba*ba*b)*a*)"
    ]
