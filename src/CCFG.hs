module CCFG where

-- conjunctive context free grammar
-- see Alexander Okhotin's work

data CCFG n t =
  CCFG
  { nonterminals :: [n]
  , terminals :: [t]
  , rules :: [CProduction n t]
  , start :: n
  }

-- rhs is interpreted as a formal intersection of symbol strings
data CProduction n t =
  CProduction 
  { lhs :: n
  , rhs :: [[Either n t]]
  }
