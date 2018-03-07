module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data Regen = Regen 
  { negate :: Bool
  , maxlength :: Int
  , timing :: Bool
  }

regen :: Parser Regen
regen = Regen
      <$> switch
          ( long "complement"
         <> short 'c'
         <> help "generate language complement" )
      <*> option auto
          ( long "maxlength"
         <> short 'm'
         <> help "maximum length of generated strings"
         <> showDefault
         <> value 6)
      <*> switch
          ( long "timing"
         <> short 't'
         <> help "switch on timing" )


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (regen <**> helper)
      ( fullDesc
     <> progDesc "blablabla"
     <> header "Generate test inputs for a regular expression" )

greet :: Regen -> IO ()
greet (Regen c m t) = putStrLn $ "Called with complement = " ++ show c ++ ", maxlength = " ++ show m ++ ", timing = " ++ show t


