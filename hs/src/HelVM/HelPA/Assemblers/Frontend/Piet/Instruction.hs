module HelVM.HelPA.Assemblers.Frontend.Piet.Instruction where

import           HelVM.HelPA.Assembler.Macro
import           HelVM.HelPA.Assembler.Value

type InstructionList = [Instruction]

type Instruction = Macro MicroInstruction

data MicroInstruction = Directive Directive | Command Command
  deriving stock (Eq , Read , Show)

data Directive =
    Label Label
  | Halt
  | Branch (Maybe Command) (Maybe BranchCondition) BranchLabel
  | BranchTable [BranchLabel]
  | PushInteger Integer
  | PushString Text
  | Print Text
  | Track
  deriving stock (Eq , Read , Show)

data Label =
    LIdentifier Identifier
  | LNatural Natural
  deriving stock (Eq , Read , Show)

data BranchCondition = BZ | BNZ | BGT | BLE
  deriving stock (Eq , Read , Show)

data BranchLabel =
    BLIdentifier Identifier
  | BLNatural (Maybe BranchIdentifierDirection) Natural
  deriving stock (Eq , Read , Show)

data BranchIdentifierDirection = Backward | Forward
  deriving stock (Eq , Read , Show)

data Command =
    Push
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | Gt
  | Ptr
  | Switch
  | Dup
  | Roll
  | InN
  | In
  | OutN
  | Out
  deriving stock (Eq , Read , Show)


