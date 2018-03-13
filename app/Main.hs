module Main where

import Data.Semigroup ((<>))
import Options.Applicative

import Data.List (sort)

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
      <*> argument str (metavar "REGEXP")


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (regen <**> helper)
      ( fullDesc
     <> progDesc "blablabla"
     <> header "Generate test inputs for a regular expression" )

greet :: Regen -> IO ()
greet (Regen c b sigma mm q t r) = do
  unless q $ putStrLn $ "Called with complement = " ++ show c ++ ", maxlength = " ++ show mm ++ ", timing = " ++ show t ++ ", REGEXP = " ++ r
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
             output = runGenerator cfg gre'
             process :: [T.Text] -> T.Text
             process
                 | q = T.pack . show . foldr (\x b -> x == x && b) True
                 | otherwise = T.unlines
         (time, _) <- T.timeItT (liftIO $ T.putStrLn (process $ concat output))
         when t $ putStrLn $ show time ++ " seconds"
