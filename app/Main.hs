module Main where

import Data.Semigroup ((<>))
import Options.Applicative

import Data.List (sort)

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.TimeIt as T

import GRegexp
import Generator
import RegexParser

data Regen = Regen
  { negate :: Bool
  , backend :: Backend
  , alphabet :: String
  , maxlength :: Maybe Int
  , quiet :: Bool
  , timing :: Bool
  , mstutter :: Maybe Int
  , timebudget :: Double
  , regexp :: String
  }

regen :: Parser Regen
regen = Regen
      <$> switch
          ( long "complement"
         <> short 'c'
         <> help "generate language complement" )
      <*> option auto
          ( long "backend"
          <> short 'b'
          <> metavar "BACKEND"
          <> value Seg
          <> help ("BACKEND algorithm used for generator: " ++ show [(minBound::Backend) .. maxBound]))
      <*> strOption
          ( long "alphabet"
            <> short 'a'
            <> metavar "ALPHABET"
            <> value "ab"
            <> help "ALPHABET used for complement" )
      <*> optional (option auto
          ( long "maxlength"
         <> short 'm'
         <> metavar "LEN"
         <> help "maximum length of generated strings"
         <> showDefault))
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "quiet operation" )
      <*> switch
          ( long "timing"
         <> short 't'
         <> help "switch on timing" )
      <*> optional (option auto
          ( long "stutter"
         <> short 's'
         <> metavar "STUTTER"
         <> help "stuttering counter for timing"
         <> showDefault))
      <*> option auto
          ( long "budget"
         <> short 'b'
         <> metavar "BUDGET"
         <> value 5.0
         <> help "time budget for stutter timing (sec)"
         <> showDefault)
      <*> argument str (metavar "REGEXP")


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (regen <**> helper)
      ( fullDesc
     <> progDesc "blablabla"
     <> header "Generate test inputs for a regular expression" )

greet :: Regen -> IO ()
greet (Regen c b sigma mm q t ms budget r) = do
  unless q $ putStrLn $ "Called with complement = " ++ show c ++ ", maxlength = " ++ show mm ++ ", timing = " ++ show t ++ ", STUTTER = " ++ show ms ++ ", REGEXP = " ++ r
  case parseRe r of
    Nothing ->
      putStrLn "Cannot parse regular expression"
    Just gre ->
      do let gre' = if c then Not gre else gre
             cfg =
                 GeneratorConfig
                 { gc_backend = b
                 , gc_maxLength = mm
                 , gc_complementAlphabet = sort sigma
                 }
             output = concat $ runGenerator cfg gre'
             process :: [T.Text] -> T.Text
             process
                 | q = T.pack . show . foldr (\x b -> x == x && b) True
                 | otherwise = T.unlines
         case ms of
           Just st
             | st > 0 -> do
                 times <- stutter budget st output
                 mapM_ (putStrLn . show) times
             | otherwise ->
                 putStrLn "Error: STUTTER size must be greater than zero"
           _ -> do
             (time, _) <- T.timeItT (liftIO $ T.putStrLn (process output))
             when t $ putStrLn $ show time ++ " seconds"

-- | perform timed, stuttering evaluation
stutter :: Double -> Int -> [T.Text] -> IO [Double]
stutter budget n xs
  | budget <= 0.0 || null xs =
    return []
stutter budget n xs = do
  (time, xs') <- T.timeItT (liftIO $ chunk n xs)
  times <- stutter (budget - time) n xs'
  return (time:times)

-- | force evaluation of a single chunk of size n
chunk n [] =
  return []
chunk n (x:xs) =
  if n == 0 then
    return xs
  else
    deepseq x (chunk (n-1) xs)
