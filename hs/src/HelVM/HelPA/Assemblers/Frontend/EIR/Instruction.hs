module HelVM.HelPA.Assemblers.Frontend.EIR.Instruction where

import           HelVM.HelPA.Assembler.Value

type InstructionList = [Instruction]

data Instruction =
    Mov !Identifier !IntegerValue
  | Add !Identifier !IntegerValue
  | Sub !Identifier !IntegerValue
  | Load !Identifier !IntegerValue
  | Store !Identifier !IntegerValue
  | PutC !IntegerValue
  | GetC !Identifier
  | Exit
  | J !Comp !Identifier !Identifier !IntegerValue
  | Jmp !Identifier
  | L !Comp !Identifier !IntegerValue
  | Dump
  | Mark !Text
  | PText
  | PData (Maybe Natural)
  | PLong !Integer
  | PString !Text
  | PFile !Natural !Text
  | PLoc !Natural !Natural !Natural
  deriving stock (Eq , Read , Show)

data Comp = CEQ | CNE | CLT | CGT | CLE | CGE
  deriving stock (Eq , Read , Show)
