module CCFGExample where

import CCFG


examplecfg :: CCFG Int Char
examplecfg =
  CCFG [0] "ab" [ CProduction 0 [[]]
                , CProduction 0 [[Right 'a', Left 0, Right 'b']]
                ] 0

anotherexample :: CCFG Int Char
anotherexample =
  CCFG [0] "()" [ CProduction 0 [[]]
                , CProduction 0 [[Right '(', Left 0, Right ')', Left 0]]
                ] 0

anbncn :: CCFG Char Char
anbncn = CCFG "SABCD" "abc"
         [ CProduction 'S' [[Left 'A', Left 'B'], [Left 'C', Left 'D']]
         , CProduction 'A' [[Right 'a', Left 'A']]
         , CProduction 'A' [[]]
         , CProduction 'B' [[Right 'b', Left 'B', Right 'c']]
         , CProduction 'B' [[]]
         , CProduction 'C' [[Right 'a', Left 'C', Right 'b']]
         , CProduction 'C' [[]]
         , CProduction 'D' [[Right 'c', Left 'D']]
         , CProduction 'D' [[]] 
         ]
         'S'

ambncmdn :: CCFG Char Char
ambncmdn = CCFG "SXYABCD" "abcd"
           [ CProduction 'S' [[Left 'X', Left 'D'], [Left 'A', Left 'Y']]
           , CProduction 'X' [[Right 'a', Left 'X', Right 'c']]
           , CProduction 'X' [[Left 'B']]
           , CProduction 'Y' [[Right 'b', Left 'Y', Right 'd']]
           , CProduction 'Y' [[Left 'C']]
           , CProduction 'A' [[Right 'a', Left 'A']]
           , CProduction 'A' [[]]
           , CProduction 'B' [[Right 'b', Left 'B']]
           , CProduction 'B' [[]]
           , CProduction 'C' [[Right 'c', Left 'C']]
           , CProduction 'C' [[]]
           , CProduction 'D' [[Right 'd', Left 'D']]
           , CProduction 'D' [[]]
           ]
           'S'
