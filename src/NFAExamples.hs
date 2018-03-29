module NFAExamples where

import qualified Data.Map.Strict as M

import NFA
import GenNFA

-- a*
nfa1 = NFA
  { nfa_states = [0]
  , nfa_sigma = "ab"
  , nfa_delta = M.insert 0 (M.insert 'a' [0] M.empty) M.empty
  , nfa_accepting = [0]
  , nfa_initial = 0
  }

-- a*b
nfa2 = NFA
  { nfa_states = [0, 1]
  , nfa_sigma = "ab"
  , nfa_delta = M.insert 0 (M.insert 'b' [1] $ M.insert 'a' [0] M.empty) M.empty
  , nfa_accepting = [1]
  , nfa_initial = 0
  }

-- ((a+b)^3)*
nfa3 = NFA
  { nfa_states = [0, 1, 2]
  , nfa_sigma = "ab"
  , nfa_delta =
    M.insert 2 (M.insert 'b' [0] $ M.insert 'a' [0] M.empty) $
    M.insert 1 (M.insert 'b' [2] $ M.insert 'a' [2] M.empty) $
    M.insert 0 (M.insert 'b' [1] $ M.insert 'a' [1] M.empty)
    M.empty
  , nfa_accepting = [0]
  , nfa_initial = 0
  }

-- w mod 3 = 0
-- r
-- 0 0 2*0+0=0
-- 0 1 2*0+1=1
-- 1 0 2*1+0=2
-- 1 1 2*1+1=0
-- 2 0 2*2+0=1
-- 2 1 2*2+1=2
nfa4 = NFA
  { nfa_states = [0, 1, 2]
  , nfa_sigma = "01"
  , nfa_delta =
    M.insert 2 (M.insert '1' [2] $ M.insert '0' [1] M.empty) $
    M.insert 1 (M.insert '1' [0] $ M.insert '0' [2] M.empty) $
    M.insert 0 (M.insert '1' [1] $ M.insert '0' [0] M.empty)
    M.empty
  , nfa_accepting = [0]
  , nfa_initial = 0
  }

-- w mod 5 = 0
nfa5 = NFA
  { nfa_states = [0, 1, 2, 3, 4]
  , nfa_sigma = "01"
  , nfa_delta =
    M.insert 4 (M.fromList [('0', [3]), ('1', [4])]) $
    M.insert 3 (M.fromList [('0', [1]), ('1', [2])]) $
    M.insert 2 (M.fromList [('0', [4]), ('1', [0])]) $
    M.insert 1 (M.fromList [('0', [2]), ('1', [3])]) $
    M.insert 0 (M.fromList [('0', [0]), ('1', [1])])
    M.empty
  , nfa_accepting = [0]
  , nfa_initial = 0
  }

-- w mod 5 = 0, w/o leading zeros
nfa6 = NFA
  { nfa_states = [0, 1, 2, 3, 4, 5, 6]
  , nfa_sigma = "01"
  , nfa_delta =
    M.insert 6 (M.fromList []) $
    M.insert 5 (M.fromList [('0', [6]), ('1', [1])]) $
    M.insert 4 (M.fromList [('0', [3]), ('1', [4])]) $
    M.insert 3 (M.fromList [('0', [1]), ('1', [2])]) $
    M.insert 2 (M.fromList [('0', [4]), ('1', [0])]) $
    M.insert 1 (M.fromList [('0', [2]), ('1', [3])]) $
    M.insert 0 (M.fromList [('0', [0]), ('1', [1])])
    M.empty
  , nfa_accepting = [0, 6]
  , nfa_initial = 5
  }

