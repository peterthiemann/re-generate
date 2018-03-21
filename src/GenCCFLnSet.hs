module GenCCFLnSet where

-- try to actually build the language level by level
-- using sets to represent levels internally

import Control.Applicative

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import CCFG
import CCFGExample

type Segments = [Lang]
type Lang = S.Set T.Text

--generate :: CFG n Char -> Segments
generate g =
  collect ntMap 0
  where
    nts = nonterminals g
    s = start g
    ntMap = foldr (\n -> M.insert n M.empty) M.empty nts
    collect mapIn level =
      run mapIn (interpretRules g level mapIn) level
    run mapIn mapOut level =
      if checkLevel mapIn mapOut level 
      then maybe [] S.toAscList (mapOut M.! s M.!? level) : collect mapOut (level + 1)
      else collect mapOut level

-- | check if languages are equal at given level 
checkLevel :: (Ord n)
  => M.Map n (M.Map Level Lang)
  -> M.Map n (M.Map Level Lang)
  -> Level
  -> Bool
checkLevel map1 map2 level =
  and $
  do nt <- M.keys map1
     return (map1 M.! nt M.!? level == map2 M.! nt M.!? level)

-- | length at which we are currently operating
type Level = Int

interpretRules :: (Ord n) 
  => CCFG n Char
  -> Level
  -> M.Map n (M.Map Level Lang) 
  -> M.Map n (M.Map Level Lang)
interpretRules g level mapIn =
  foldr interpretRule mapIn (rules g)
  where
    interpretRule rule mapIn =
      let nt = lhs rule
          ruleMapIn = mapIn M.! nt
          result = evaluateRule mapIn level rule
          alterLevel Nothing = Just result
          alterLevel (Just lang) = Just (S.union lang result)
          ruleMapOut = M.alter alterLevel level ruleMapIn
      in  
        M.insert nt ruleMapOut mapIn

evaluateRule :: (Ord n) => M.Map n (M.Map Level Lang) -> Level -> CProduction n Char -> Lang
evaluateRule mapIn level rule =
  foldr1 S.intersection $ do
  onerhs <- rhs rule
  return $ foldr S.union S.empty $ traverseRhs mapIn level onerhs

langEmpty :: Lang
langEmpty = S.singleton T.empty

dictEmpty :: M.Map Level Lang
dictEmpty = M.insert 0 langEmpty M.empty

traverseRhs :: (Ord n) => M.Map n (M.Map Level Lang) -> Level -> [Either n Char] -> [Lang]
traverseRhs mapIn 0 [] = return langEmpty
traverseRhs mapIn n [] = fail "premature end of rhs"
traverseRhs mapIn n (Right t : vs) | n > 0 = liftA (S.mapMonotonic (T.cons t)) $ traverseRhs mapIn (n-1) vs
traverseRhs mapIn n (Left nt : vs) = do
  let mapNt = mapIn M.! nt
  k <- filter (<=n) $ M.keys mapNt
  let lntk = mapNt M.! k
  lvs <- traverseRhs mapIn (n-k) vs
  return $ S.fromAscList $ do { wntk <- S.toAscList lntk
                              ; wvs <- S.toAscList lvs
                              ; return $ T.append wntk wvs }
traverseRhs mapIn _ _ = fail "end of rhs not reached"
