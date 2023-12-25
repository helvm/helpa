module HelVM.HelPA.Assemblers.SblAsm.Instruction where

import           HelVM.HelPA.Assembler.Value

-- Current grammar
--program	      := {instruction}
--instruction   := [id ':' ] [command]
--command       := data | code
--data          := 'data' value
--code          := 'subleq' value value [value]
--value	        := number | id


-- Types
--type InstructionList = [Instruction]

--data Instruction = Instruction (Maybe Label) (Maybe Command)
--  deriving stock (Eq , Read , Show)

data Command =
    Directive Directive
  | Command Identifier [ValueInteger]

data Directive =
    Include String
  | Word (NonEmpty Integer)
  | Equ Identifier Integer
  | Ascii String
  | Asciiz String
  | Macro Identifier [Param]
  | Endm
  | Ifdef
  | Else
  | Endif

type Param = Identifier
