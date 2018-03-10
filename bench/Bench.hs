module Main where

import Criterion.Main

import GRegexp
import Generator
import RegexParser

regexEnv :: (GRE Char -> GRE Char) -> String -> GRE Char
regexEnv f input =
    case parseRe input of
      Nothing -> error "Invalid regex"
      Just ok -> f ok

matchGroup :: Maybe Int -> String -> (GRE Char -> GRE Char) -> String -> Benchmark
matchGroup maxLength complementAlphabet f input =
     env (pure $ regexEnv f input) $ \ ~re ->
     bgroup ("generation for " ++ pretty re ++ " with maxLen=" ++ show maxLength ++ " and complement alpha=" ++ complementAlphabet) $
     flip map [minBound..maxBound] $ \gen ->
     let config =
             GeneratorConfig
             { gc_backend = gen
             , gc_maxLength = maxLength
             , gc_complementAlphabet = complementAlphabet
             }
     in bench ("gen=" ++ show gen) $ nf (runGenerator config) re

main :: IO ()
main =
    defaultMain
    [ matchGroup (Just 4) "ab" id "a*b"
    , matchGroup (Just 4) "ab" Not "a*b"
    , matchGroup (Just 8) "ab" Not "a*b"
    , matchGroup (Just 16) "ab" Not "a*b"
    , matchGroup (Just 20) "ab" Not "a*b"
    ]
