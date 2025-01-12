module HelVM.HelPA.Assemblers.Frontend.HPL.Instruction where

type InstructionList = [Instruction]


data Instruction
  = Import Text
  | Def Bool Text [Text] Text
  | Asm Bool Text [Text] Text
  deriving stock (Show)

