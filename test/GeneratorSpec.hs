{-# LANGUAGE OverloadedStrings #-}
module GeneratorSpec (spec) where

import GRegexp
import Generator
import RegexParser

import Test.Hspec
import qualified Data.Text as T

testCase ::
    Maybe Int
    -> String
    -> (GRE Char -> GRE Char)
    -> String
    -> ([[T.Text]] -> IO ())
    -> Spec
testCase maxLen complementAlphabet f input test =
    case parseRe input of
        Nothing -> fail "Invalid regular expression"
        Just parsedRe ->
            do let re = f parsedRe
               describe ("generates correct outputs for " ++ pretty re ++ " with maxLnegth=" ++ show maxLen ++ " and complementAlphabet=" ++ show complementAlphabet) $
                   flip mapM_ [minBound..maxBound] $ \gen ->
                   do let config =
                              GeneratorConfig
                              { gc_backend = gen
                              , gc_maxLength = maxLen
                              , gc_complementAlphabet = complementAlphabet
                              }
                      it ("using " ++ show gen) $
                          test (runGenerator config re)

spec :: Spec
spec =
    do testCase (Just 4) "ab" id "a*b" $ \res ->
           res `shouldBe`
               [[],["b"],["ab"],["aab"]]
       testCase (Just 4) "ab" Not "a*b" $ \res ->
           res `shouldBe`
               [[""],["a"],["aa","ba","bb"],["aaa","aba","abb","baa","bab","bba","bbb"]]
       testCase (Just 4) "ab" id "~(a*)|a*"
         (`shouldBe` [[""]
                     ,["a","b"]
                     ,["aa","ab","ba","bb"]
                     ,["aaa", "aab" ,"aba","abb","baa","bab","bba","bbb"]])
       testCase (Just 4) "ab" id "~(a*)&a*"
         (`shouldBe` [[], [], [], []])
