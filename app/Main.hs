module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Control.Monad
import Control.Monad.IO.Class

import System.TimeIt as T

import GRegexp
import GenSegments
import qualified GenString as G

data Regen = Regen 
  { negate :: Bool
  , alphabet :: String
  , maxlength :: Maybe Int
  , quiet :: Bool
  , timing :: Bool
  , regexp :: String
  }

regen :: Parser Regen
regen = Regen
      <$> switch
          ( long "complement"
         <> short 'c'
         <> help "generate language complement" )
      <*> strOption
          ( long "alphabet"
            <> short 'a'
            <> metavar "ALPHABET"
            <> value "ab"
            <> help "ALPHABET used for complement" )
      <*> optional (option auto
          ( long "maxlength"
         <> short 'm'
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
      <*> argument str (metavar "REGEXP")


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (regen <**> helper)
      ( fullDesc
     <> progDesc "blablabla"
     <> header "Generate test inputs for a regular expression" )

greet :: Regen -> IO ()
greet (Regen c al mm q t r) = do
  unless q $ putStrLn $ "Called with complement = " ++ show c ++ ", maxlength = " ++ show mm ++ ", timing = " ++ show t ++ ", REGEXP = " ++ r
  -- when q $ putStrLn "quiet operation"
  let result = star $ G.segmentize ["a", "ab", "aba"]
      output1
        | c = complementSegs al result
        | otherwise = result
      output2 = case mm of
                  Nothing -> output1
                  Just m  -> take (m+1) output1
  (time, _) <- T.timeItT (liftIO $ mapM_ putStrLn (concat output2))
  when t $ putStrLn $ show time ++ " seconds"
