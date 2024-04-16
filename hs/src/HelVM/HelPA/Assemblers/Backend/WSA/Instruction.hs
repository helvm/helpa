module HelVM.HelPA.Assemblers.Backend.WSA.Instruction where

import           HelVM.HelPA.Assembler.Value

pushLiteral :: Integer -> Instruction
pushLiteral = Push

----
data Instruction =
-- Stack instructions
    Push  !Integer
  | Pop
  | Dup
  | Swap
-- Arithmetic
  | Add
  | Sub
  | Mul
  | Div
  | Mod
-- Heap access
  | Store
  | Load
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
  | EOL
  deriving stock (Eq , Read , Show)

type InstructionList = [Instruction]

--instance DSL Instruction where
--  movi s d = do
--    push $ Value $ unImmediate s
--

