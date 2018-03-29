module GenNFA where

import Types
import NFA

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T

import Debug.Trace

generate :: (Ord q, Show q) => NFA q Char -> Segments
generate nfa =
  loop statemap
  where
    initialstate = nfa_initial nfa
    states = nfa_states nfa
    accepting = nfa_accepting nfa
    delta = nfa_delta nfa
    init True = S.singleton T.empty -- accepting
    init False = S.empty       -- rejecting
    statemap = foldr (\q -> M.insert q (init (q `elem` accepting))) M.empty states
    sigma = L.sort $ nfa_sigma nfa
    loop statemap = 
      let
        g q m =
          let
            delta_q = fromMaybe M.empty (delta M.!? q)
            successors = [S.mapMonotonic (T.cons t) (statemap M.! q) 
                         | t <- sigma, q <- fromMaybe [] (delta_q M.!? t)]
            successors' =
              M.foldrWithKey
              (\t qs -> (++) [S.mapMonotonic (T.cons t) (statemap M.! q) | q <- qs])
              [] delta_q
          in
            M.insert q (foldr S.union S.empty successors') m
        statemap' = foldr g M.empty states 
      in
        S.toAscList (statemap M.! initialstate) :
        loop statemap'


