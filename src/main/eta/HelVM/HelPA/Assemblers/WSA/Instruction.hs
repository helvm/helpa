module HelVM.HelPA.Assemblers.WSA.Instruction where

import Numeric.Natural

data Instruction = 
    Push Natural
  | PushS String
  
  | Pop
  | Doub
  | Swap
  
  | Add (Maybe Natural)
  | Sub (Maybe Natural)
  | Mul (Maybe Natural)
  | Div (Maybe Natural)
  | Mod (Maybe Natural)
  | Store (Maybe Natural)
  | Retrieve (Maybe Natural)

  | Mark Identifier
  | Call Identifier
  | Jump Identifier
  | JumpZ Identifier
  | JumpN Identifier
  | JumpP Identifier
  | JumpNZ Identifier
  | JumpPZ Identifier
  | JumpPN Identifier
  | JumpNP Identifier
  | Include Identifier
  
  | Ret
  | Exit
  | OutN
  | OutC
  | InN
  | InC
  
  | Test Natural
  | ValueString Identifier String
  | ValueInteger Identifier Natural
  | IfOption Identifier
  | ElseOption
  | EndOption
  | ElseIfOption Identifier
  deriving (Eq, Show, Ord)
  
  

type InstructionList = [Instruction]

type NaturalValue = Either Identifier Natural

type Identifier = String
