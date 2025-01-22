module HelVM.HelPA.Assemblers.Common.Macro where

import           HelVM.HelPA.Assembler.Value

-- It is a tree
data Macro i =
    Macro [NaturalValue] Identifier [i]
  | Micro i
  deriving stock (Eq , Read , Show)
