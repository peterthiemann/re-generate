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
          <> value SegStar
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
          ( long "limit"
         <> short 'l'
         <> metavar "LIMIT"
         <> value 5.0
         <> help "time limit for stutter timing (sec)"
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
  unless q $ putStrLn $ "# Called with complement = " ++ show c ++ ", maxlength = " ++ show mm ++ ", timing = " ++ show t ++ ", STUTTER = " ++ show ms ++ ", REGEXP = " ++ r
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
             | st > 0 -> stutter budget st output
             | otherwise ->
                 putStrLn "Error: STUTTER size must be greater than zero"
           _ -> do
             (time, _) <- T.timeItT (liftIO $ T.putStrLn (process output))
             when t $ putStrLn $ show time ++ " seconds"

showCsvTime i d = show i ++ "\t" ++ show d
                  
-- | perform timed, stuttering evaluation
stutter :: Double -> Int -> [T.Text] -> IO ()
stutter budget step xs = loop 0 0 xs
    where loop t n xs =
            if (t >= budget || null xs) then return ()
            else do
                (time, xs') <- T.timeItT (liftIO $ chunk step xs)
                let t' = t + time
                times <- loop t' (n+step) xs'
                putStrLn $ showCsvTime n t' 
                return ()

-- | force evaluation of a single chunk of size n
chunk n [] =
  return []
chunk n (x:xs) =
  if n == 0 then
    return xs
  else
    deepseq x (chunk (n-1) xs)
