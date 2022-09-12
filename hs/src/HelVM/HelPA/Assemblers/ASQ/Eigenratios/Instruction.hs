module HelVM.HelPA.Assemblers.ASQ.Eigenratios.Instruction where

--import           HelVM.HelPA.Assembler.Extra
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

type LabelList = [Label]
type Label = Identifier

data Command = Data IntegerValue | Code IntegerValue IntegerValue (Maybe IntegerValue)
  deriving stock (Eq , Read , Show)
