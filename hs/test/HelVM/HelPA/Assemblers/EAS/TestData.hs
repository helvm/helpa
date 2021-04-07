module HelVM.HelPA.Assemblers.EAS.TestData where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Value

trueIL :: InstructionList
trueIL = []

----

helloIL :: InstructionList
helloIL =
  [N (Literal 32),R
  ,N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,O,O,O,O,O,O,O,R
  ,N (Literal 10),R
  ,N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),R
  ,O,O,O,O,O,O,O,R
  ]

----

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


pipILReduced :: InstructionList
pipILReduced =
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

pip2IL :: InstructionList
pip2IL =
  [L "LOOP",I,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Variable "WRITE"),T,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,N (Literal 1),N (Variable "LOOP"),T,R
  ]

pip2ILReduced :: InstructionList
pip2ILReduced =
  [L "LOOP",I,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 2),T,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,N (Literal 1),N (Literal 1),T,R]

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

reverseILReduced :: InstructionList
reverseILReduced =
  [N (Literal 0),N (Literal 1),S,R
  ,L "READ",I,R
  ,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 2),T,R
  ,N (Literal 0),N (Literal 1),H,T,R
  ,L "WRITE",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,O,N (Literal 1),N (Literal 5),T,R]

----

functionIL :: InstructionList
functionIL =
  [L "ADD",N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 1),H,R
  ,S,R
  ,S,R
  ,N (Literal 1),R
  ,N (Literal 2),H,R
  ,T,R]

----

addIL :: InstructionList
addIL =
  [I,R
  ,I,R
  ,A,N (Literal 1),N (Variable "ADD"),T,R
  ,O,R
  ,N (Literal 1),N (Literal 0),T,R]

addILLinked = addIL <> functionIL

addILReduced :: InstructionList
addILReduced =
  [I,R
  ,I,R
  ,A,N (Literal 1),N (Literal 6),T,R
  ,O,R
  ,N (Literal 1),N (Literal 0),T,R
  ,L "ADD",N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 1),H,R
  ,S,R
  ,S,R
  ,N (Literal 1),R
  ,N (Literal 2),H,R
  ,T,R]

----

writeStrIL :: InstructionList
writeStrIL =
  [L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Variable "WRITESTR"),T,R
  ]

writeStrILReduced :: InstructionList
writeStrILReduced =
  [L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 1),T,R
  ]

----

hello2IL :: InstructionList
hello2IL =
  [N (Literal 0),N (Literal 10),N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),N (Literal 32),N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

hello2ILLinked = hello2IL <> writeStrIL

hello2ILReduced :: InstructionList
hello2ILReduced =
  [N (Literal 0),N (Literal 10),N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),N (Literal 32),N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,A,N (Literal 1),N (Literal 4),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ,L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 4),T,R
  ]

----

hello4IL :: InstructionList
hello4IL =
  [N (Literal 0),N (Literal 10),U "Hello, world!",R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

hello4ILLinked = hello4IL <> writeStrIL

----

writeNumIL :: InstructionList
writeNumIL =
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

writeNumILLinked = writeNumIL <> writeStrIL

writeNumILReduced :: InstructionList
writeNumILReduced =
  [L "WRITENUM",N (Literal 0),N (Literal 2),H,R
  ,N (Literal 0),H,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 48),O,T,N (Literal 1),N (Literal 1),H,T,R
  ,L "WRITENUMloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 11),T,R
  ,N (Literal 10),E,R
  ,N (Literal 0),N (Literal 48),S,S,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Literal 4),T,R
  ,L "WRITENUMdone",A,N (Literal 1),N (Literal 13),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 13),T,R
  ]

----

multiplyIL :: InstructionList
multiplyIL =
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

multiplyILReduced :: InstructionList
multiplyILReduced =
  [L "MULTIPLY",N (Literal 2),H,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,L "MULTIPLYloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Literal 15),T,R
  ,N (Literal 1),S,R
  ,N (Literal 2),H,R
  ,N (Literal 0),N (Literal 0),N (Literal 3),S,H,R
  ,S,S,R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 1),N (Literal 5),T,R
  ,L "MULTIPLYdone",N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R
  ]

----

readNumIL :: InstructionList
readNumIL =
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

readNumILLinked = readNumIL <> multiplyIL

readNumILReduced :: InstructionList
readNumILReduced =
 [L "READNUM",L "READNUMspace",I,R
  ,N (Literal 0),H,N (Literal 32),S,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),T,N (Literal 1),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,L "READNUMloop",N (Literal 48),N (Literal 32),S,R
  ,N (Literal 1),H,R
  ,N (Literal 10),A,N (Literal 1),N (Literal 19),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,S,S,R
  ,I,R
  ,N (Literal 0),H,N (Literal 32),S,N (Literal 14),T,R
  ,N (Literal 1),N (Literal 17),T,R
  ,L "READNUMnl",N (Literal 0),H,N (Literal 10),S,N (Literal 16),T,R
  ,N (Literal 1),N (Literal 17),T,R
  ,L "READNUMeof",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 6),T,R
  ,L "READNUMdone",N (Literal 0),N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R
  ,L "MULTIPLY",N (Literal 2),H,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,L "MULTIPLYloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Literal 33),T,R
  ,N (Literal 1),S,R
  ,N (Literal 2),H,R
  ,N (Literal 0),N (Literal 0),N (Literal 3),S,H,R
  ,S,S,R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 1),N (Literal 23),T,R
  ,L "MULTIPLYdone",N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R]

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
  ]

factILLinked = factIL <> readNumIL <> writeNumIL <> multiplyIL <> writeStrIL

factILReduced :: InstructionList
factILReduced =
  [A,N (Literal 1),N (Literal 14),T,R
  ,A,N (Literal 1),N (Literal 5),T,R
  ,A,N (Literal 1),N (Literal 32),T,R
  ,N (Literal 10),O,N (Literal 1),N (Literal 0),T,R
  ,L "FACT",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,N (Literal 1),S,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Literal 13),T,R
  ,N (Literal 0),H,R
  ,N (Literal 1),S,R
  ,A,N (Literal 1),N (Literal 5),T,R
  ,A,N (Literal 1),N (Literal 44),T,R
  ,L "FACTdone",N (Literal 1),N (Literal 2),H,T,R
  ,L "READNUM",L "READNUMspace",I,R
  ,N (Literal 0),H,N (Literal 32),S,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),T,N (Literal 14),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,L "READNUMloop",N (Literal 48),N (Literal 32),S,R
  ,N (Literal 1),H,R
  ,N (Literal 10),A,N (Literal 1),N (Literal 44),T,R
  ,N (Literal 0),N (Literal 1),H,R
  ,S,S,R
  ,I,R
  ,N (Literal 0),H,N (Literal 32),S,N (Literal 27),T,R
  ,N (Literal 1),N (Literal 30),T,R
  ,L "READNUMnl",N (Literal 0),H,N (Literal 10),S,N (Literal 29),T,R
  ,N (Literal 1),N (Literal 30),T,R
  ,L "READNUMeof",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 19),T,R
  ,L "READNUMdone",N (Literal 0),N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R
  ,L "WRITENUM",N (Literal 0),N (Literal 2),H,R
  ,N (Literal 0),H,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 48),O,T,N (Literal 1),N (Literal 1),H,T,R
  ,L "WRITENUMloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 42),T,R
  ,N (Literal 10),E,R
  ,N (Literal 0),N (Literal 48),S,S,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Literal 35),T,R
  ,L "WRITENUMdone",A,N (Literal 1),N (Literal 60),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "MULTIPLY",N (Literal 2),H,N (Literal 2),H,R
  ,N (Literal 0),R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,L "MULTIPLYloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Literal 58),T,R
  ,N (Literal 1),S,R
  ,N (Literal 2),H,R
  ,N (Literal 0),N (Literal 0),N (Literal 3),S,H,R
  ,S,S,R
  ,N (Literal 2),H,R
  ,N (Literal 2),H,R
  ,N (Literal 1),N (Literal 48),T,R
  ,L "MULTIPLYdone",N (Literal 1),H,T,R
  ,N (Literal 1),N (Literal 2),H,T,R
  ,L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 60),T,R
  ]

----

bottlesIL :: InstructionList
bottlesIL = [N (Literal 1),N (Variable "MAIN"),T,R
  ,D "writestr.eas",D "writenum.eas",R
  ,L "nBoB",N (Literal 1),H,R
  ,A,N (Literal 1),N (Variable "WRITENUM"),T,R
  ,N (Literal 0),N (Literal 114),N (Literal 101),N (Literal 101),N (Literal 98),N (Literal 32),N (Literal 102),N (Literal 111),N (Literal 32),N (Literal 115),N (Literal 101),N (Literal 108),N (Literal 116),N (Literal 116),N (Literal 111),N (Literal 98),N (Literal 32),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "nBoBotW",N (Literal 1),H,R
  ,A,N (Literal 1),N (Variable "nBoB"),T,R
  ,N (Literal 0),N (Literal 108),N (Literal 108),N (Literal 97),N (Literal 119),N (Literal 32),N (Literal 101),N (Literal 104),N (Literal 116),N (Literal 32),N (Literal 110),N (Literal 111),N (Literal 32),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "MAIN",N (Literal 3),R
  ,L "LOOP",N (Literal 0),H,A,N (Literal 1),N (Variable "nBoBotW"),T,R
  ,N (Literal 44),O,N (Literal 32),O,R
  ,N (Literal 0),H,A,N (Literal 1),N (Variable "nBoB"),T,R
  ,N (Literal 10),O,R
  ,N (Literal 0),N (Literal 10),N (Literal 100),N (Literal 110),N (Literal 117),N (Literal 111),N (Literal 114),N (Literal 97),N (Literal 32),N (Literal 116),N (Literal 105),N (Literal 32),N (Literal 115),N (Literal 115),N (Literal 97),N (Literal 112),N (Literal 32),N (Literal 44),N (Literal 110),N (Literal 119),N (Literal 111),N (Literal 100),N (Literal 32),N (Literal 101),N (Literal 110),N (Literal 111),N (Literal 32),N (Literal 101),N (Literal 107),N (Literal 97),N (Literal 84),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),S,R
  ,N (Literal 0),H,A,N (Literal 1),N (Variable "nBoBotW"),T,R
  ,N (Literal 10),N (Literal 10),O,O,R
  ,N (Literal 0),H,N (Variable "LOOP"),T,R
  ,N (Literal 0),T,R]

bottlesILLinked :: InstructionList
bottlesILLinked = [N (Literal 1),N (Variable "MAIN"),T,R
  ,L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,L "WRITENUM",N (Literal 0),N (Literal 2),H,R
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
  ,R
  ,L "nBoB",N (Literal 1),H,R
  ,A,N (Literal 1),N (Variable "WRITENUM"),T,R
  ,N (Literal 0),N (Literal 114),N (Literal 101),N (Literal 101),N (Literal 98),N (Literal 32),N (Literal 102),N (Literal 111),N (Literal 32),N (Literal 115),N (Literal 101),N (Literal 108),N (Literal 116),N (Literal 116),N (Literal 111),N (Literal 98),N (Literal 32),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "nBoBotW",N (Literal 1),H,R
  ,A,N (Literal 1),N (Variable "nBoB"),T,R
  ,N (Literal 0),N (Literal 108),N (Literal 108),N (Literal 97),N (Literal 119),N (Literal 32),N (Literal 101),N (Literal 104),N (Literal 116),N (Literal 32),N (Literal 110),N (Literal 111),N (Literal 32),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "MAIN",N (Literal 3),R
  ,L "LOOP",N (Literal 0),H,A,N (Literal 1),N (Variable "nBoBotW"),T,R
  ,N (Literal 44),O,N (Literal 32),O,R
  ,N (Literal 0),H,A,N (Literal 1),N (Variable "nBoB"),T,R
  ,N (Literal 10),O,R
  ,N (Literal 0),N (Literal 10),N (Literal 100),N (Literal 110),N (Literal 117),N (Literal 111),N (Literal 114),N (Literal 97),N (Literal 32),N (Literal 116),N (Literal 105),N (Literal 32),N (Literal 115),N (Literal 115),N (Literal 97),N (Literal 112),N (Literal 32),N (Literal 44),N (Literal 110),N (Literal 119),N (Literal 111),N (Literal 100),N (Literal 32),N (Literal 101),N (Literal 110),N (Literal 111),N (Literal 32),N (Literal 101),N (Literal 107),N (Literal 97),N (Literal 84),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),S,R
  ,N (Literal 0),H,A,N (Literal 1),N (Variable "nBoBotW"),T,R
  ,N (Literal 10),N (Literal 10),O,O,R
  ,N (Literal 0),H,N (Variable "LOOP"),T,R
  ,N (Literal 0),T,R]

bottlesILReduced :: InstructionList
bottlesILReduced = [N (Literal 1),N (Literal 31),T,R
  ,L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 2),T,R
  ,L "WRITENUM",N (Literal 0),N (Literal 2),H,R
  ,N (Literal 0),H,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 48),O,T,N (Literal 1),N (Literal 1),H,T,R
  ,L "WRITENUMloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 18),T,R
  ,N (Literal 10),E,R
  ,N (Literal 0),N (Literal 48),S,S,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Literal 11),T,R
  ,L "WRITENUMdone",A,N (Literal 1),N (Literal 2),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,R
  ,L "nBoB",N (Literal 1),H,R
  ,A,N (Literal 1),N (Literal 8),T,R
  ,N (Literal 0),N (Literal 114),N (Literal 101),N (Literal 101),N (Literal 98),N (Literal 32),N (Literal 102),N (Literal 111),N (Literal 32),N (Literal 115),N (Literal 101),N (Literal 108),N (Literal 116),N (Literal 116),N (Literal 111),N (Literal 98),N (Literal 32),R
  ,A,N (Literal 1),N (Literal 2),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "nBoBotW",N (Literal 1),H,R
  ,A,N (Literal 1),N (Literal 21),T,R
  ,N (Literal 0),N (Literal 108),N (Literal 108),N (Literal 97),N (Literal 119),N (Literal 32),N (Literal 101),N (Literal 104),N (Literal 116),N (Literal 32),N (Literal 110),N (Literal 111),N (Literal 32),R
  ,A,N (Literal 1),N (Literal 2),T,R
  ,N (Literal 1),N (Literal 1),H,T,R
  ,L "MAIN",N (Literal 3),R
  ,L "LOOP",N (Literal 0),H,A,N (Literal 1),N (Literal 26),T,R
  ,N (Literal 44),O,N (Literal 32),O,R
  ,N (Literal 0),H,A,N (Literal 1),N (Literal 21),T,R
  ,N (Literal 10),O,R
  ,N (Literal 0),N (Literal 10),N (Literal 100),N (Literal 110),N (Literal 117),N (Literal 111),N (Literal 114),N (Literal 97),N (Literal 32),N (Literal 116),N (Literal 105),N (Literal 32),N (Literal 115),N (Literal 115),N (Literal 97),N (Literal 112),N (Literal 32),N (Literal 44),N (Literal 110),N (Literal 119),N (Literal 111),N (Literal 100),N (Literal 32),N (Literal 101),N (Literal 110),N (Literal 111),N (Literal 32),N (Literal 101),N (Literal 107),N (Literal 97),N (Literal 84),R
  ,A,N (Literal 1),N (Literal 2),T,R
  ,N (Literal 1),S,R
  ,N (Literal 0),H,A,N (Literal 1),N (Literal 26),T,R
  ,N (Literal 10),N (Literal 10),O,O,R
  ,N (Literal 0),H,N (Literal 32),T,R
  ,N (Literal 0),T,R]

----

euclidIL :: InstructionList
euclidIL =
  [L "EUCLID",N (Literal 2),H,N (Literal 2),H,R
  ,L "EUCLIDloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Variable "EUCLIDdone"),T,R
  ,N (Literal 0),N (Literal 1),S,H,R
  ,E,R
  ,N (Literal 0),N (Literal 2),H,T,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Variable "EUCLIDloop"),T,R
  ,L "EUCLIDdone",N (Literal 0),T,R
  ,N (Literal 1),N (Literal 2),H,R
  ,T,R
  ]
  
euclidILReduced :: InstructionList
euclidILReduced =
  [L "EUCLID",N (Literal 2),H,N (Literal 2),H,R
  ,L "EUCLIDloop",N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 1),N (Literal 10),T,R
  ,N (Literal 0),N (Literal 1),S,H,R
  ,E,R
  ,N (Literal 0),N (Literal 2),H,T,R
  ,N (Literal 1),H,R
  ,N (Literal 1),N (Literal 2),T,R
  ,L "EUCLIDdone",N (Literal 0),T,R
  ,N (Literal 1),N (Literal 2),H,R
  ,T,R
  ]
