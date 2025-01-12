module HelVM.HelPA.Assemblers.Frontend.FBF.Instruction where

import           HelVM.HelPA.Assembler.Value

type InstructionList = [Instruction]

data Instruction
    = Compiler CompilerInstruction
    | Code CodeInstruction
    deriving stock (Show, Eq)

data CompilerInstruction
    = Echo
    | ByteCells
    | EndBlock
    | LineBreaks Natural
    | Custom Natural
    | LineMode LineMode
    | Table Natural Text
    | Dim [Identifier]
    | Block Identifier [Identifier] InstructionList
    deriving stock (Show, Eq)

data LineMode = Linux | Mac | Dos
  deriving stock (Show, Eq)

data CodeInstruction
    = Bell
    | End
    | Line
    | Rem
    | Space
    | Tab

    | MsgClear Integer
    | Read Identifier
    | ClearStack Identifier
    | BrainFuck Text
    | MoveFrom IntegerValue

    | Copy Identifier Identifier
    | CopySize Identifier Identifier
    | Pop Identifier Identifier

    | Dec Integer Identifier
    | Inc Integer Identifier
    | Set Integer Identifier

    | Push IntegerValue Identifier

    | RTable IntegerValue Identifier Identifier
    | WTable IntegerValue IntegerValue Identifier

    | Add IntegerValue IntegerValue Identifier
    | Sub IntegerValue IntegerValue Identifier
    | Multi IntegerValue IntegerValue Identifier
    | Div IntegerValue IntegerValue Identifier
    | Mod IntegerValue IntegerValue Identifier
    | Comp IntegerValue IntegerValue Identifier

    | UnEq IntegerValue Identifier InstructionList
    | IfEq IntegerValue Identifier InstructionList
    | IfNotEq IntegerValue Identifier InstructionList

    | Byte2Ascii IntegerValue Identifier Identifier Identifier
    | Ascii2Byte IntegerValue IntegerValue IntegerValue Identifier
    | Print [Identifier]
    | Msg [Text]
    | Call Identifier [IntegerValue]

    deriving stock (Show, Eq)
