{-# LANGUAGE OverloadedStrings #-}
module FinitenessSpec (spec) where

import GRegexp
import Generator
import RegexParser

import Data.List (sort)
import Test.Hspec
import qualified Data.Text as T

testCase1 ::
    Maybe Int
    -> String
    -> (GRE Char)
    -> ([[T.Text]] -> IO ())
    -> Spec
testCase1 maxLen complementAlphabet re test =
    do describe ("generates correct outputs for " ++ pretty re ++ " with maxLength=" ++ show maxLen ++ " and complementAlphabet=" ++ show complementAlphabet) $
            flip mapM_ [minBound..maxBound] $ \gen ->
            do let config =
                    GeneratorConfig
                    { gc_backend = gen
                    , gc_maxLength = maxLen
                    , gc_complementAlphabet = complementAlphabet
                    }
               it ("using " ++ show gen) $
                   test (map sort $ runGenerator config re)

spec :: Spec
spec =
    do testCase1 (Just 5) "ab" (Star One) $ \res ->
           res `shouldBe`
               [[""]]
       testCase1 (Just 5) "ab" (Star (Or Zero One)) $ \res ->
           res `shouldBe`
               [[""]]
    --    testCase1 Nothing "ab" id (Star One) $ \res -> -- these do not terminate
    --        res `shouldBe`
    --            [[""]]
    --    testCase1 Nothing "ab" id (Star (Or Zero One)) $ \res ->
    --        res `shouldBe`
    --            [[""]]