module CFG where

-- context free grammar

data CFG n t =
  CFG
  { nonterminals :: [n]
  , terminals :: [t]
  , rules :: [Production n t]
  , start :: n
  }

data Production n t =
  Production 
  { lhs :: n
  , rhs :: [Either n t]
  }


  
