module HelVM.HelPA.Assemblers.Frontend.WSA.Instruction where

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Collections.SList

pushLiteral :: Integer -> Instruction
pushLiteral = Push

pushSLiteral :: SString -> Instruction
pushSLiteral = PushS

----
data Instruction =
-- Stack instructions
    Push  !Integer
  | PushS !SString
  | Pop
  | Dup
  | Swap
-- Arithmetic
  | Add !(Maybe Integer)
  | Sub !(Maybe Integer)
  | Mul !(Maybe Integer)
  | Div !(Maybe Integer)
  | Mod !(Maybe Integer)
-- Heap access
  | Store !(Maybe Integer)
  | Load  !(Maybe Integer)
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

--instance DSL Instruction where
--  movi s d = do
--    push $ Value $ unImmediate s
--

