module Main where

import GRegexp
import GenSegments
import qualified GenString as G

testsize :: Int
testsize = 6

main :: IO ()
main = do
  putStrLn "Test suite running"
  putStrLn "*** a_star"
  putStrLn $ show $ take testsize ex_a_star
  putStrLn "*** a_ab_aba_star"
  putStrLn $ show $ take testsize ex_a_ab_aba_star
  putStrLn "*** abstarstar"
  putStrLn $ show $ take testsize ex_abstarstar
  putStrLn "*** done"


-- experiments

lang_a = G.segmentize ["a"]
lang_a_ab_aba = G.segmentize ["a", "ab", "aba"]
lang_b = G.segmentize ["b"]

ex_a_star = star lang_a
ex_a_ab_aba_star = star lang_a_ab_aba
ex_abstarstar = star (concatenate lang_a (star lang_b))
