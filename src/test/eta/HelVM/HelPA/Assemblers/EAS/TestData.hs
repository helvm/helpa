module HelVM.HelPA.Assemblers.EAS.TestData where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Value

trueIL :: InstructionList
trueIL = []

trueETA = ""

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

helloETA = "Niie\nNsaeNatseNatoeNatoeNahoeNtoae\nOOOOOOO\nNtoe\nNineNahaeNatoeNaaaeNatseNaohe\nOOOOOOO\n"

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

pipETA = "I\nNeH\nNeNteSS\nNtheT\nNeNteHT\nNteNeT\nO\nNteNteT\n"

----

pip2IL :: InstructionList
pip2IL =
  [L "LOOP",I,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Variable "WRITE"),T,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,N (Literal 1),N (Variable "LOOP"),T,R
  ]

pip2IL' :: InstructionList
pip2IL' =
  [L "LOOP",I,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 2),T,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,L "WRITE",O,N (Literal 1),N (Literal 1),T,R]

pip2ETA = "INeHNeNteSSNaeTNeNteHTNteNeT\nONteNteT\n"

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

reverseIL' :: InstructionList
reverseIL' =
  [N (Literal 0),N (Literal 1),S,R
  ,L "READ",I,R
  ,N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,N (Literal 2),T,R
  ,N (Literal 0),N (Literal 1),H,T,R
  ,L "WRITE",N (Literal 0),H,N (Literal 0),N (Literal 1),S,S,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,N (Literal 0),N (Literal 1),H,T,N (Literal 1),N (Literal 0),T,R
  ,O,N (Literal 1),N (Literal 5),T,R]

reverseETA = "NeNteS\nI\nNeHNeNteSSNaeT\nNeNteHT\nNeHNeNteSSANeNteSST\nNeNteHTNteNeT\nONteNneT\n"

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

functionETA = "NaeH\nNaeH\nNe\nNteH\nS\nS\nNte\nNaeH\nT\n"

----
addIL :: InstructionList
addIL =
  [I,R
  ,I,R
  ,A,N (Literal 1),N (Variable "ADD"),T,R
  ,O,R
  ,N (Literal 1),N (Literal 0),T,R]
  
addIL' :: InstructionList
addIL' =
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

addETA = "I\nI\nANteNseT\nO\nNteNeT\nNaeH\nNaeH\nNe\nNteH\nS\nS\nNte\nNaeH\nT\n"

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

writeStrIL' :: InstructionList
writeStrIL' =
  [L "WRITESTR",N (Literal 1),H,R
  ,N (Literal 0),H,R
  ,A,N (Literal 0),N (Literal 1),S,S,T,R
  ,S,N (Literal 1),N (Literal 1),H,T,R
  ,O,R
  ,N (Literal 1),N (Literal 1),T,R
  ]

writeStrETA = "NteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNteT\n"

----

hello2IL :: InstructionList
hello2IL =
  [N (Literal 0),N (Literal 10),N (Literal 33),N (Literal 100),N (Literal 108),N (Literal 114),N (Literal 111),N (Literal 119),N (Literal 32),N (Literal 44),N (Literal 111),N (Literal 108),N (Literal 108),N (Literal 101),N (Literal 72),R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

hello2IL' :: InstructionList
hello2IL' =
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

hello2ETA = "NeNtoeNineNahaeNatoeNaaaeNatseNaoheNiieNsaeNatseNatoeNatoeNahoeNtoae\nANteNieT\nNteNeT\nNteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNieT\n"
  
----

hello4IL :: InstructionList
hello4IL =
  [N (Literal 0),N (Literal 10),U "Hello, world!",R
  ,A,N (Literal 1),N (Variable "WRITESTR"),T,R
  ,N (Literal 1),N (Literal 0),T,R
  ]

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

writeNumIL' :: InstructionList
writeNumIL' =
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

writeNumETA = "NeNaeH\nNeHANeNteSST\nNsseOTNteNteHT\nNeH\nANeNteSST\nSNteNtieT\nNtoeE\nNeNsseSS\nNteH\nNteNieT\nANteNtseT\nNteNteHT\nNteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNtseT\n"

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

multiplyIL' :: InstructionList
multiplyIL' =
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

multiplyETA = "NaeHNaeH\nNe\nNaeH\nNaeH\nNeH\nANeNteSST\nNteNateT\nNteS\nNaeH\nNeNeNoeSH\nSS\nNaeH\nNaeH\nNteNneT\nNteHT\nNteNaeHT\n"

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

readNumIL' :: InstructionList
readNumIL' =
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

readNumETA = "I\nNeHNiieS\nANeNteSST\nNeNteTNteT\nNeNteH\nNsseNiieS\nNteH\nNtoeANteNaneT\nNeNteH\nSS\nI\nNeHNiieSNaheT\nNteNaoeT\nNeHNtoeSNaaeT\nNteNaoeT\nNeHNeNteSSNseT\nNeNteHT\nNteNaeHT\nNaeHNaeH\nNe\nNaeH\nNaeH\nNeH\nANeNteSST\nNteNineT\nNteS\nNaeH\nNeNeNoeSH\nSS\nNaeH\nNaeH\nNteNoaeT\nNteHT\nNteNaeHT\n"

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

factIL' :: InstructionList
factIL' = 
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

factETA = "ANteNaheT\nANteNneT\nANteNiieT\nNtoeONteNeT\nNteH\nNeH\nNteSANeNteSST\nNteNtseT\nNeH\nNteS\nANteNneT\nANteNsaeT\nNteNaeHT\nI\nNeHNiieS\nANeNteSST\nNeNteTNaheT\nNeNteH\nNsseNiieS\nNteH\nNtoeANteNsaeT\nNeNteH\nSS\nI\nNeHNiieSNoseT\nNteNiaeT\nNeHNtoeSNiteT\nNteNiaeT\nNeHNeNteSSNaneT\nNeNteHT\nNteNaeHT\nNeNaeH\nNeHANeNteSST\nNsseOTNteNteHT\nNeH\nANeNteSST\nSNteNsheT\nNtoeE\nNeNsseSS\nNteH\nNteNnheT\nANteNttieT\nNteNteHT\nNaeHNaeH\nNe\nNaeH\nNaeH\nNeH\nANeNteSST\nNteNttaeT\nNteS\nNaeH\nNeNeNoeSH\nSS\nNaeH\nNaeH\nNteNsseT\nNteHT\nNteNaeHT\nNteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNttieT\n"

----

bottlesIL :: InstructionList
bottlesIL =
  [N (Literal 1),N (Variable "MAIN"),T,R
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
  ,N (Literal 0),T,R
  ]

bottlesETA = "NteNioeT\nNteH\nNeH\nANeNteSST\nSNteNteHT\nO\nNteNaeT\nNeNaeH\nNeHANeNteSST\nNsseOTNteNteHT\nNeH\nANeNteSST\nSNteNaieT\nNtoeE\nNeNsseSS\nNteH\nNteNtieT\nANteNaeT\nNteNteHT\n\nNteH\nANteNtteT\nNeNaaaeNahoeNahoeNahheNiieNahieNatseNiieNaaoeNahoeNatoeNaaieNaaieNatseNahheNiie\nANteNaeT\nNteNteHT\nNteH\nANteNoheT\nNeNatoeNatoeNtsseNaoheNiieNahoeNahseNaaieNiieNatneNatseNiie\nANteNaeT\nNteNteHT\nNoe\nNeHANteNoneT\nNsaeONiieO\nNeHANteNoheT\nNtoeO\nNeNtoeNahaeNatneNaaneNatseNaaaeNtsseNiieNaaieNatheNiieNaaoeNaaoeNtsseNaaheNiieNsaeNatneNaoheNatseNahaeNiieNahoeNatneNatseNiieNahoeNataeNtsseNtnhe\nANteNaeT\nNteS\nNeHANteNoneT\nNtoeNtoeOO\nNeHNiieT\nNeT\n"

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
  
euclidIL' :: InstructionList
euclidIL' =
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

euclidETA = "NaeHNaeH\nNeH\nANeNteSST\nNteNtoeT\nNeNteSH\nE\nNeNaeHT\nNteH\nNteNaeT\nNeT\nNteNaeH\nT\n"
