module HelVM.HelPA.Assemblers.Frontend.SblAsm.Instruction where

import           HelVM.HelPA.Assembler.Value

-- Current grammar
--program	      := {instruction}
--instruction   := [id ':' ] [command]
--command       := data | code
--data          := 'data' value
--code          := 'subleq' value value [value]
--value	        := number | id


-- Types
type InstructionList = [Instruction]

data Instruction = Instruction (Maybe Label) (Maybe Command)
  deriving stock (Eq , Read , Show)

data Command =
    Directive Directive
  | Command Identifier [IntegerValue]
  | Code Identifier [IntegerValue]
  deriving stock (Eq , Read , Show)

data Directive =
    Include String
  | Word (NonEmpty Integer)
  | Equ Identifier Integer
  | Ascii String
  | Asciiz String
  | Macro Identifier [Identifier]
  | Endm
  | Ifdef
  | Else
  | Endif
  deriving stock (Eq , Read , Show)

type Label = Identifier
