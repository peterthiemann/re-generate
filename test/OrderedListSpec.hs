{-# LANGUAGE OverloadedStrings #-}
module OrderedListSpec where

import OrderedLists

import Test.Hspec

spec :: Spec
spec =
    do describe "difference" $
           do it "empty l" $
                  do let r = [] `difference` ["a", "b", "c"]
                     r `shouldBe` []
              it "empty r" $
                  do let r = ["a", "b", "c"] `difference` []
                     r `shouldBe` ["a", "b", "c"]
              it "trivial case" $
                  do let r = ["a", "b", "c"] `difference` ["a", "b", "c"]
                     r `shouldBe` []
              it "trivial case 2" $
                  do let r = ["a", "b", "c"] `difference` ["a", "b", "c", "d"]
                     r `shouldBe` []
              it "trivial case 3" $
                  do let r = ["a", "b", "c", "d"] `difference` ["a", "b", "c"]
                     r `shouldBe` ["d"]
              it "trivial case 4" $
                  do let r = ["a", "b", "d"] `difference` ["a", "b", "f"]
                     r `shouldBe` ["d"]
              it "trivial case 5" $
                  do let r = ["a", "b", "d", "l", "r"] `difference` ["a", "b", "f"]
                     r `shouldBe` ["d", "l", "r"]
              it "repeated case 1" $
                  do let r = ["a", "a", "b", "l", "r"] `difference` ["a", "b", "f"]
                     r `shouldBe` ["a", "l", "r"]
              it "repeated case 2" $
                  do let r = ["a", "a", "b", "b", "l", "r"] `difference` ["a", "b", "f"]
                     r `shouldBe` ["a", "b", "l", "r"]
              it "repeated case 2" $
                  do let r =
                             ["a", "a", "a", "b", "b", "l", "r"] `difference`
                             ["a", "a", "b", "f"]
                     r `shouldBe` ["a", "b", "l", "r"]
