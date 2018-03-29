module NFA where

import qualified Data.Map.Strict as M

data NFA q t=
  NFA 
  { nfa_states :: [q]
  , nfa_sigma :: [t]
  , nfa_delta :: M.Map q (M.Map t [q])
  , nfa_accepting :: [q]
  , nfa_initial :: q
  }
