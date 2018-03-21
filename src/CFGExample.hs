module CFGExample where

import CFG


examplecfg :: CFG Int Char
examplecfg =
  CFG [0] "ab" [Production 0 [], Production 0 [Right 'a', Left 0, Right 'b']] 0

anotherexample :: CFG Int Char
anotherexample =
  CFG [0] "()" [Production 0 [], Production 0 [Right '(', Left 0, Right ')', Left 0]] 0
