module Generator
    ( GeneratorConfig (..)
    , Backend(..)
    , runGenerator
    )
where

import GRegexp
import RegexParser
import Types (Alphabet)
import qualified GenNaive as GN
import qualified GenNaiveStar as GNS
import qualified GenRefined as GR
import qualified GenRefinedStar as GRS
import qualified GenSegments as GS1
import qualified GenSegmentsStar2 as GS2
import qualified GenSegmentsConvolution as GSC
import qualified GenSet as GS
import qualified GenString as G

import qualified Data.Set as S
import qualified Data.Text as T

data GeneratorConfig
    = GeneratorConfig
    { gc_backend :: !Backend
    , gc_maxLength :: !(Maybe Int)
    , gc_complementAlphabet :: !Alphabet
    } deriving (Show, Eq)

data Backend
    = Naive
    | NaiveStar
    | Seg
    | SegStar
    | SegConv
    | Ref
    | RefStar
    | NaiveSet
    deriving (Read, Show, Enum, Bounded, Eq)

runGenerator :: GeneratorConfig -> GRE Char -> [[T.Text]]
runGenerator gc re =
    let r = generate (gc_backend gc) (gc_complementAlphabet gc) re
    in case gc_maxLength gc of
         Just l -> take l r
         Nothing -> r

generate :: Backend -> Alphabet -> GRE Char -> [[T.Text]]
generate Seg = GS1.generate'
generate SegStar = GS2.generate'
generate SegConv = GSC.generate'
generate Ref = GR.generate'
generate RefStar = GRS.generate'
generate Naive = GN.generate'
generate NaiveStar = GNS.generate'
generate NaiveSet = GS.generate'
