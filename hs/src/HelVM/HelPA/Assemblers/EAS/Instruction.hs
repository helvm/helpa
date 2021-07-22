module HelVM.HelPA.Assemblers.EAS.Instruction where

import           HelVM.Common.Collections.SList

import           HelVM.HelPA.Assembler.Value

data Instruction
  = E
  | T
  | A
  | O
  | I
  | N !NaturalValue
  | S
  | H
  | R
  | D !Identifier
  | L !Identifier
  | U !SString
  deriving stock (Eq , Show , Ord)

type InstructionList = [Instruction]
