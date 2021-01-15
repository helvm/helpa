module HelVM.HelPA.Assemblers.WSA.Instruction where

import HelVM.HelPA.Common.Value

data Instruction = 
-- Stack instructions
    Push IntegerValue
  | PushS StringValue
  | Pop
  | Dup
  | Swap
--Arithmetic  
  | Add (Maybe IntegerValue)
  | Sub (Maybe IntegerValue)
  | Mul (Maybe IntegerValue)
  | Div (Maybe IntegerValue)
  | Mod (Maybe IntegerValue)
-- Heap access
  | Store (Maybe IntegerValue)
  | Load (Maybe IntegerValue)
-- Control
  | Mark Identifier
  | Call Identifier
  | Branch Identifier
  | BranchZ Identifier
  | BranchN Identifier
  | BranchP Identifier
  | BranchNZ Identifier
  | BranchPZ Identifier
  | BranchPN Identifier
  | BranchNP Identifier
  | Return
  | End
-- IO instructions  
  | OutputChar
  | OutputNum
  | InputChar
  | InputNum
-- Other instructions
  | Noop
  | DebugPrintStack
  | DebugPrintHeap
-- Pseudo instructions
  | Include Identifier
  | Test Integer
  | ValueString Identifier String
  | ValueInteger Identifier Natural
  | IfOption Identifier
  | ElseOption
  | EndOption
  | ElseIfOption Identifier
  deriving (Eq, Show, Ord)

type InstructionList = [Instruction]
