module HelVM.HelPA.Assemblers.WSA.Instruction where

import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Collections.SList

pushLiteral :: Integer -> Instruction
pushLiteral = Push . Literal

pushSLiteral :: SString -> Instruction
pushSLiteral = PushS . Literal

----
data Instruction =
-- Stack instructions
    Push  !IntegerValue
  | PushS !StringValue
  | Pop
  | Dup
  | Swap
-- Arithmetic
  | Add !(Maybe IntegerValue)
  | Sub !(Maybe IntegerValue)
  | Mul !(Maybe IntegerValue)
  | Div !(Maybe IntegerValue)
  | Mod !(Maybe IntegerValue)
-- Heap access
  | Store !(Maybe IntegerValue)
  | Load  !(Maybe IntegerValue)
-- Control
  | Mark     !Identifier
  | Call     !Identifier
  | Branch   !Identifier
  | BranchZ  !Identifier
  | BranchM  !Identifier
  | BranchP  !Identifier
  | BranchNP !Identifier
  | BranchNM !Identifier
  | BranchNZ !Identifier
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
  | Include      !Identifier
  | Test         !Integer
  | ValueString  !Identifier !SString
  | ValueInteger !Identifier !Natural
  | IfOption     !Identifier
  | ElseOption
  | EndOption
  | ElseIfOption !Identifier
  | EOL
  deriving stock (Eq , Read , Show)

type InstructionList = [Instruction]
