module HelVM.HelPA.Assemblers.EAS.Instruction where

import           HelVM.HelIO.Collections.SList

import           HelVM.HelPA.Assembler.Value

labelToIdentifiers :: Instruction -> [Identifier]
labelToIdentifiers (L s) = [s]
labelToIdentifiers  _    = []

type InstructionList = [Instruction]

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
  deriving stock (Eq , Read , Show)
