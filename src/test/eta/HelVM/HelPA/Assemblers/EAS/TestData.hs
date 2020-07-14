{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.EAS.TestData where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Value

import qualified Data.Text as T

trueIL :: InstructionList
trueIL = []

helloEAS :: T.Text
helloEAS = T.unlines
  [ "Number 32 # can't quote a space character: use ASCII value"
  , "Number ',  Number 'o  Number 'l  Number 'l  Number 'e  Number 'H"
  , "Output Output Output Output Output Output Output"
  , "Number 10 # can't quote a newline either"
  , "Number '!  Number 'd  Number 'l  Number 'r  Number 'o  Number 'w"
  , "Output Output Output Output Output Output Output"
  ]

helloIL :: InstructionList
helloIL =
  [N (Literal 32),R
  ,N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,O,O,O,O,O,O,O,R,N (Literal 10),R
  ,N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),R
  ,O,O,O,O,O,O,O,R
  ]

helloETA :: String
helloETA = "Niie\nNsaeNatseNatoeNatoeNahoeNtoae\nOOOOOOO\nNtoe\nNineNahaeNatoeNaaaeNatseNaohe\nOOOOOOO\n"

----

pipEAS :: T.Text
pipEAS = T.unlines
  [ ">LOOP: Input                          # Read a character"
  , "Number 0  Halibut                     # Duplicate it for the test"
  , "Number 0  Number 1  Subtract Subtract # Compare with -1 (EOF)"
  , "Number <WRITE  Transfer               # If not equal, continue ..."
  , "N 0 N 1 H T                           # ... otherwise discard -1"
  , "Number 1  Number 0  Transfer          # ... and make a successful exit"
  , ">WRITE: Output                        # Copy the character"
  , "Number 1  Number <LOOP  Transfer      # Go back and read the next one"
  ]

pipIL :: InstructionList
pipIL =
  [L "LOOP",I,R
  ,N (Literal 0),H,R
  ,N (Literal 0),N (Literal 1),S,S,R
  ,N (Variable "WRITE"),T,R
  ,N (Literal 0),N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,R
  ,N (Literal 1),N (Variable "LOOP"),T,R
  ]


pipIL' :: InstructionList
pipIL' =
  [L "LOOP",I,R
  ,N (Literal 0),H,R
  ,N (Literal 0),N (Literal 1),S,S,R
  ,N (Literal 7),T,R
  ,N (Literal 0),N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,R
  ,N (Literal 1),N (Literal 1),T,R
  ]

----

pipETA :: String
pipETA = "I\nNeH\nNeNteSS\nNtheT\nNeNteHT\nNteNeT\nO\nNteNteT\n"

----

pip2IL :: InstructionList
pip2IL =
   [L "LOOP",I,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Variable "WRITE"),T,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
   ,L "WRITE",O,N (Literal 1),N (Variable "LOOP"),T,R
   ]


----

functionDefinitionEAS :: T.Text
functionDefinitionEAS = T.unlines
  ["# On entry, stack = ... x y addr"
  ,"# Prologue: roll args above return address"
  ,">ADD: Number 2 Halibut          # ... y addr x"
  ,"Number 2  Halibut               # ... addr x y"
   
  ,"# Add the numbers: subtract y from zero, then the result from a."
  ,"Number 0                        # ... addr x y 0"
  ,"Number 1  Halibut               # ... addr x 0 y"
  ,"Subtract                        # ... addr x -y"
  ,"Subtract                        # ... addr x+y"
   
  ,"# Epilogue: tidy up stack to leave result on top, then return"
  ,"Number 1                        # ... addr x+y TRUE"
  ,"Number 2  Halibut               # ... x+y TRUE addr"
  ,"Transfer                        # Return to addr, leaving acc on stack]"
  ]

functionDefinitionIL :: InstructionList
functionDefinitionIL = 
  [L "ADD",N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 1),H,R
  ,S,R,S,R
  ,N (Literal 1),R
  ,N (Literal 2),H,R
  ,T,R]

----

functionCallEAS :: T.Text
functionCallEAS = T.unlines
  ["Input                            # stack: x"
  ,"Input                            # stack: x y"
  ,"Address N 1  N <ADD  Transfer"
  ,"Output                           # print result"
  ,"Number 1  Number 0  Transfer     # jump to location zero, i.e. stop"]

functionCallIL :: InstructionList
functionCallIL = 
  [I,R
  ,I,R
  ,A,N (Literal 1),N (Variable "ADD"),T,R
  ,O,R
  ,N (Literal 1),N (Literal 0),T,R]
  
functionCallAndDefETA :: String
functionCallAndDefETA = "I\nI\nANteNseT\nO\nNteNeT\nNaeH\nNaeH\nNe\nNteH\nS\nS\nNte\nNaeH\nT\n"

----

writingAStringEAS :: T.Text
writingAStringEAS = T.unlines
  [">WRITESTR: N1 Halibut                   # move next character above addr"
  ,"N0 Halibut                              # duplicate it for the test below"
  ,"Address N0 N1 S S T                     # skip if non-zero"

  ,"# We only get here if we've seen the NUL, and by that point, the stack"
  ,"# is: ... <addr> NUL.  We need to strip the trailing NUL and return."
  ,"# This epilogue is in the middle of the function (why not?)"
  ,"Subtract N1 N1 Halibut Transfer"

  ,"# The character is on top of the stack output and loop."
  ,"# (Or think of it as tail-recursion if you're a Lisp hacker :-)"
  ,"Output"
  ,"N1 N<WRITESTR Transfer"
  ]

writingAStringIL :: InstructionList
writingAStringIL = 
  [L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Variable "WRITESTR"),T,R
  ]

----

hello2EAS :: T.Text
hello2EAS = T.unlines
  ["N0 N10 N'! N'd N'l N'r N'o N'w N32 N', N'o N'l N'l N'e N'H"
  ,"Address N1 N<WRITESTR Transfer"
  ,"N1 N0 T"
  ]

hello2IL :: InstructionList
hello2IL =
  [N (Literal 0),N (Literal 10),N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),N (Literal 32),N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

hello2ETA :: String
hello2ETA =
  "NeNtoeNineNahaeNatoeNaaaeNatseNaoheNiieNsaeNatseNatoeNatoeNahoeNtoae\nANteNieT\nNteNeT\nNteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNieT\n"
  
----

charBetterHelloWorldEAS :: T.Text
charBetterHelloWorldEAS = T.unlines
  ["0 10 '! 'd 'l 'r 'o 'w 32 ', 'o 'l 'l 'e 'H"
  ,"Address N1 N<WRITESTR Transfer"
  ,"N1 N0 T"
  ]

----

stringBetterHelloWorldEAS :: T.Text
stringBetterHelloWorldEAS = T.unlines
  ["0 10 \"Hello, world!\" "
  ,"Address N1 N<WRITESTR Transfer"
  ,"N1 N0 T"
  ]

stringBetterHelloWorldIL :: InstructionList
stringBetterHelloWorldIL =
  [N (Literal 0),N (Literal 10),U "Hello, world!",R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

----

writingANumberEAS :: T.Text
writingANumberEAS = T.unlines
  ["# Prologue (called with arg)."
  ,"# Start state                           # ... arg addr"
  ,">WRITENUM: N0 N2 Halibut                # ... addr NUL arg"
    
  ,"N0 H A N0 N1 S S T                      # skip to real start if non-zero"
  ,"N'0 O T N1 N1 Halibut Transfer          # special case for arg=0"
    
  ,"# Invariant: stack contains a NUL, then the ASCII codes of the digits"
  ,"# rendered so far (least significant first), then the remaining part"
  ,"# of the number being rendered."
  ,">WRITENUMloop: N0 Halibut               # ... addr NUL <digits> arg arg"
  ,"Address N0 N1 S S Transfer              # if not done, skip a line"
  ,"S N1 N<WRITENUMdone Transfer            # done: strip NUL, jump to output phase"
  ,"N10 dividE                              # find next digit"
  ,"N0 N'0 Subtract Subtract                # calculate its ASCII code"
  ,"# Stack now contains \0 <digits> quotient ASCII(remainder)"
  ,"# We just need to swap the last two to attain the invariant above."
  ,"N1 Halibut                              # swap"
  ,"N1 N<WRITENUMloop Transfer              # next iteration"
    
  ,"# Call existing code to output the accumulated digits."
  ,">WRITENUMdone: A N1 N<WRITESTR Transfer"
  ,"# Epilogue: return to the stacked address"
  ,"N1 N1 Halibut Transfer"
  ]

writingANumberIL :: InstructionList
writingANumberIL =
  [L "WRITENUM",N (Literal 0),N (Literal 2),H,R
  ,N (Literal 0),H,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 48),O,T,N (Literal 1),N (Literal 1),H,T,R
  ,L "WRITENUMloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Variable "WRITENUMdone"),T,R
  ,N (Literal 10),E,R
  ,N (Literal 0),N (Literal 48),S,S,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Variable "WRITENUMloop"),T,R
  ,L "WRITENUMdone",A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ]

----

multiplicationEAS :: T.Text
multiplicationEAS = T.unlines
  ["# Prologue for a function of two arguments"
  ,"# Initial state:                # ... x y addr"
  ,">MULTIPLY: N2 H N2 H                # ... addr x y"
    
  ,"# Create an accumulator (initially zero), push it below the working numbers."
  ,"N0                                # ... addr x y acc"
  ,"N2 Halibut                        # ... addr y acc x"
  ,"N2 Halibut                        # ... addr acc x y"
    
  ,"# Loop invariant: stack is of the form: ... addr acc x y'"
  ,"# Where y' is gradually decremented, and acc increases by x each time."
  ,">MULTIPLYloop: N0 Halibut        # dup"
  ,"A N0 N1 S S T                        # if not done, skip a line"
  ,"N1 N<MULTIPLYdone Transfer        # done: jump to end"
  ,"N1 Subtract                        # ... addr acc x y  (but y--)"
  ,"N2 Halibut                        # ... addr x y acc"
  ,"N0 N0 N3 Subtract Halibut        # ... addr x y acc 0 x"
  ,"Subtract Subtract                # ... addr x y acc  (but acc+=x)"
  ,"N2 Halibut                        # ... addr y acc x"
  ,"N2 Halibut                        # ... addr acc x y"
  ,"N1 N<MULTIPLYloop Transfer        # next"
    
  ,"# Stack is: ... addr acc x y(=0); we have to discard two values"
  ,">MULTIPLYdone: N1 H T                # ... addr acc"
    
  ,"# Standard function-call epilogue."
  ,"N1 N2 H T"
  ]

multiplicationIL :: InstructionList
multiplicationIL =
   [L "MULTIPLY",N (Literal 2),H,N (Literal 2),H,R
   ,N (Literal 0),R
   ,N (Literal 2),H,R
   ,N (Literal 2),H,R
   ,L "MULTIPLYloop",N (Literal 0),H,R
   ,A,N (Literal 0),N (Literal 1),S,S,T,R
   ,N (Literal 1),N (Variable "MULTIPLYdone"),T,R
   ,N (Literal 1),S,R
   ,N (Literal 2),H,R
   ,N (Literal 0),N (Literal 0),N (Literal 3),S,H,R
   ,S,S,R
   ,N (Literal 2),H,R
   ,N (Literal 2),H,R
   ,N (Literal 1),N (Variable "MULTIPLYloop"),T,R
   ,L "MULTIPLYdone",N (Literal 1),H,T,R
   ,N (Literal 1),N (Literal 2),H,T,R
   ]

----

readingANumberEAS :: T.Text
readingANumberEAS = T.unlines
  ["# Requires: MULTIPLY"
    
  ,"# No input args => no function prologue"
    
  ,"# Skip leading spaces"
  ,">READNUM: >READNUMspace: Input"
  ,"N0 Halibut N32 Subtract                        # duplicate input, compare to SPACE"
  ,"A N0 N1 S S Transfer                        # Skip if non-zero (i.e. != ' ')"
  ,"N0 N1 T N<READNUMspace T                # discard NUL and loop"
    
  ,"# Initialise accumulator and push it below initial digit"
  ,"N0 N1 Halibut"
    
  ,"# Accumulate digits: we already have the first"
  ,">READNUMloop: N'0' Subtract                # convert char to number"
  ,"N1 Halibut                                # stack: num acc"
  ,"N10 A N1 N<MULTIPLY Transfer                # multiply by 10"
  ,"N0 N1 Halibut                                # stack: num 0 10*acc"
  ,"Subtract Subtract                        # add accumular back onto new digit"
  ,"Input"
  ,"N0 H N32 S N<READNUMnl T                # if not a space, continue"
  ,"N1 N<READNUMdone T                        # it _was_ a space: out of here"
  ,">READNUMnl: N0 H N10 S N<READNUMeof T        # if not a newline, continue"
  ,"N1 N<READNUMdone T                        # it _was_ a newline: out of here"
  ,">READNUMeof: N0 H N0 N1 S S N<READNUMloop T        # if not a EOF, loop"
    
  ,"# Otherwise, we're finished: discard spare space character"
  ,">READNUMdone: N0 N1 H T"
    
  ,"# epilogue"
  ,"N1 N2 H T"
  ]

readingANumberIL :: InstructionList
readingANumberIL =
  [L "READNUM",L "READNUMspace",I,R
  ,N (Literal 0),H,N (Literal 32),S,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),T,N (Variable "READNUMspace"),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,L "READNUMloop",N (Literal 48),N (Literal 32),S,R
  ,N (Literal 1),H,R
  ,N (Literal 10),A,N (Literal 1),N (Variable "MULTIPLY"),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,S,S,R
  ,I,R
  ,N (Literal 0),H,N (Literal 32),S,N (Variable "READNUMnl"),T,R
  ,N (Literal 1),N (Variable "READNUMdone"),T,R
  ,L "READNUMnl",N (Literal 0),H,N (Literal 10),S,N (Variable "READNUMeof"),T,R
  ,N (Literal 1),N (Variable "READNUMdone"),T,R
  ,L "READNUMeof",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Variable "READNUMloop"),T,R
  ,L "READNUMdone",N (Literal 0),N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R
  ]

----


reverseIL :: InstructionList
reverseIL =
  [N (Literal 0),N (Literal 1),S,R
  ,L "READ",I,R
  ,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Variable "READ"),T,R
  ,N (Literal 0),N (Literal 1),H,T,R
  ,L "WRITE",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,O,N (Literal 1),N (Variable "WRITE"),T,R
  ]

----

factIL :: InstructionList
factIL =
  [A,N (Literal 1),N (Variable "READNUM"),T,R
  ,A,N (Literal 1),N (Variable "FACT"),T,R
  ,A,N (Literal 1),N (Variable "WRITENUM"),T,R
  ,N (Literal 10),O,N (Literal 1),N (Literal 0),T,R
  ,L "FACT",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,N (Literal 1),S,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Variable "FACTdone"),T,R
  ,N (Literal 0),H,R
  ,N (Literal 1),S,R
  ,A,N (Literal 1),N (Variable "FACT"),T,R
  ,A,N (Literal 1),N (Variable "MULTIPLY"),T,R
  ,L "FACTdone",N (Literal 1),N (Literal 2),H,T,R
  ,D "readnum"
  ]

----

bottlesIL :: InstructionList
bottlesIL =
  [N (Literal 1),N (Variable "MAIN"),T,R
  ,D "writestr"
  ]