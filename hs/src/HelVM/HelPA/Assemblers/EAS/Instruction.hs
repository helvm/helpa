module HelVM.HelPA.Assemblers.EAS.Instruction where

import HelVM.HelPA.Common.Value

data Instruction = E | T | A | O | I | N NaturalValue | S | H | R | D Identifier | L Identifier | U String
  deriving (Eq, Show, Ord)

type InstructionList = [Instruction]

isLabel :: Instruction -> Bool
isLabel (L _) = True
isLabel  _    = False
