module HelVM.HelPA.Assembler.Macro where

import           HelVM.HelPA.Assembler.Value

data Macro a =
    Micro a
  | Call [IntegerValue] Identifier
  | Def [Identifier] Identifier [Macro a]
  deriving stock (Eq , Read , Show)
