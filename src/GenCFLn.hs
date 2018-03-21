module GenCFLn where

-- try to actually build the language level by level

import Control.Applicative

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import CFG
import CFGExample

type Segments = [Lang]
type Lang = [T.Text]

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
      then maybe [] id (mapOut M.! s M.!? level) : collect mapOut (level + 1)
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
  => CFG n Char
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
          alterLevel (Just lang) = Just (L.union lang result)
          ruleMapOut = M.alter alterLevel level ruleMapIn
      in  
        M.insert nt ruleMapOut mapIn

evaluateRule :: (Ord n) => M.Map n (M.Map Level Lang) -> Level -> Production n Char -> Lang
evaluateRule mapIn level rule =
  foldr L.union [] $ traverseRhs mapIn level (rhs rule)

langEmpty :: M.Map Level Lang
langEmpty = M.insert 0 [T.empty] M.empty

traverseRhs :: (Ord n) => M.Map n (M.Map Level Lang) -> Level -> [Either n Char] -> [Lang]
traverseRhs mapIn 0 [] = return [T.empty]
traverseRhs mapIn n [] = fail "premature end of rhs"
traverseRhs mapIn n (Right t : vs) | n > 0 = liftA (map (T.cons t)) $ traverseRhs mapIn (n-1) vs
traverseRhs mapIn n (Left nt : vs) = do
  let mapNt = mapIn M.! nt
  k <- filter (<=n) $ M.keys mapNt
  let lntk = mapNt M.! k
  lvs <- traverseRhs mapIn (n-k) vs
  return $ liftA2 T.append lntk lvs
traverseRhs mapIn _ _ = fail "end of rhs not reached"

