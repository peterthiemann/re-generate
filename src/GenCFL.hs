module GenCFL where

import Control.Applicative

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import CFG

type Segments = [Lang]
type Lang = [T.Text]

--generate :: CFG n Char -> Segments
generate g =
  collect nsMap 0
  where
    ns = nonterminals g
    s = start g
    nsMap = foldr (\n -> M.insert n M.empty) M.empty ns
    collect mapIn level =
      run mapIn (interpretRules g mapIn) level
    run mapIn mapOut level =
      if checkLevel mapIn mapOut level 
      then maybe [] id (mapOut M.! s M.!? level) : run mapIn mapOut (level + 1)
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
  -> M.Map n (M.Map Level Lang) 
  -> M.Map n (M.Map Level Lang)
interpretRules g mapIn =
  foldr interpretRule mapIn (rules g)
  where
    interpretRule rule mapIn =
      let nt = lhs rule
          ruleMap = mapIn M.! nt
          result = evaluateRule mapIn rule
          updater = Just . M.unionWith L.union result
      in  
        M.update updater nt mapIn


evaluateRule ::(Ord n) => M.Map n (M.Map Level Lang) -> Production n Char -> M.Map Level Lang
evaluateRule mapIn rule =
  foldr concatLang (M.insert 0 [T.empty] M.empty) $
  map g (rhs rule)
  where
    g (Left n)  = mapIn M.! n
    g (Right t) = M.insert 1 [T.singleton t] M.empty


concatLang :: M.Map Level Lang -> M.Map Level Lang -> M.Map Level Lang
concatLang lx ly =
  foldr update M.empty $
  do lenx <- M.keys lx
     leny <- M.keys ly
     return (lenx + leny, liftA2 T.append (lx M.! lenx) (ly M.! leny))
  where
    update (len, lang) = M.alter (updater lang) len
    updater lang Nothing = Just lang
    updater lang (Just lang') = Just (L.union lang lang')

examplecfg =
  CFG [0] "ab" [Production 0 [], Production 0 [Right 'a', Left 0, Right 'b']] 0

anotherexample =
  CFG [0] "()" [Production 0 [], Production 0 [Right '(', Left 0, Right ')', Left 0]] 0
