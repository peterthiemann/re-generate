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

matchGroup :: Maybe Int -> Maybe Int -> String -> (GRE Char -> GRE Char) -> String -> Benchmark
matchGroup maxLength maxWords complementAlphabet f input =
     env (pure $ regexEnv f input) $ \ ~re ->
     bgroup (infoString re) $
     flip map [minBound..maxBound] $ \gen ->
     let config =
             GeneratorConfig
             { gc_backend = gen
             , gc_maxLength = maxLength
             , gc_complementAlphabet = complementAlphabet
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

main :: IO ()
main =
    defaultMain
    [ matchGroup (Just 4) Nothing "ab" id "a*b"
    , matchGroup (Just 8) Nothing "ab" id "~(a*)|a*"
    , matchGroup (Just 8) Nothing "ab" id "~(a*)&a*"
    , matchGroup (Just 4) Nothing "ab" Not "a*b"
    , matchGroup (Just 8) Nothing "ab" Not "a*b"
    , matchGroup (Just 16) Nothing "ab" Not "a*b"
    , matchGroup (Just 20) Nothing "ab" Not "a*b"
    , matchGroup (Just 20) (Just 20) "ab" Not "a*b"
    , matchGroup (Just 20) (Just 40) "ab" Not "a*b"
    , matchGroup (Just 20) (Just 80) "ab" Not "a*b"
    ]
