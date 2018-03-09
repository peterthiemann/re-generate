module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Control.Monad
import Control.Monad.IO.Class

import System.TimeIt as T

import GRegexp
import qualified GenSegments as GS1
import qualified GenString as G
import RegexParser

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
      <*> argument str (metavar "REGEXP")


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (regen <**> helper)
      ( fullDesc
     <> progDesc "blablabla"
     <> header "Generate test inputs for a regular expression" )

greet :: Regen -> IO ()
greet (Regen c sigma mm q t r) = do
  unless q $ putStrLn $ "Called with complement = " ++ show c ++ ", maxlength = " ++ show mm ++ ", timing = " ++ show t ++ ", REGEXP = " ++ r
  case parseRe r of
    Nothing ->
      putStrLn "Cannot parse regular expression"
    Just gre -> do
      let
        gre' = if c then Not gre else gre
        result = GS1.generate' sigma gre'
        output2 = case mm of
                    Nothing -> result
                    Just m  -> take (m+1) result
        process
          | q = show . foldr (\x b -> x == x && b) True
          | otherwise = unlines
      (time, _) <- T.timeItT (liftIO $ putStrLn (process $ concat output2))
      when t $ putStrLn $ show time ++ " seconds"
