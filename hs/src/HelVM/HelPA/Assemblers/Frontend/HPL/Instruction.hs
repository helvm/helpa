module HelVM.HelPA.Assemblers.Frontend.HPL.Instruction where

data Element
  = Asm Bool Text [Text] Text
  | Section Text
  deriving stock (Show)
