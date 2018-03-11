module Generator
    ( GeneratorConfig (..)
    , Backend(..)
    , runGenerator
    )
where

import GRegexp
import RegexParser
import qualified GenNaive as GN
import qualified GenNaiveStar as GNS
import qualified GenRefined as GR
import qualified GenRefinedStar as GRS
import qualified GenSegments as GS1
import qualified GenSegmentsStar2 as GS2
import qualified GenString as G

data GeneratorConfig t
    = GeneratorConfig
    { gc_backend :: !Backend
    , gc_maxLength :: !(Maybe Int)
    , gc_complementAlphabet :: ![t]
    } deriving (Show, Eq)

data Backend
    = Naive
    | NaiveStar
    | Seg
    | SegStar
    | Ref
    | RefStar
    deriving (Read, Show, Enum, Bounded, Eq)

runGenerator :: Ord t => GeneratorConfig t -> GRE t -> [[[t]]]
runGenerator gc re =
    let r = generate (gc_backend gc) (gc_complementAlphabet gc) re
    in case gc_maxLength gc of
         Just l -> take l r
         Nothing -> r

generate :: (Ord t) => Backend -> [t] -> GRE t -> [[[ t ]]]
generate Seg = GS1.generate'
generate SegStar = GS2.generate'
generate Ref = GR.generate'
generate RefStar = GRS.generate'
generate Naive = GN.generate'
generate NaiveStar = GNS.generate'
