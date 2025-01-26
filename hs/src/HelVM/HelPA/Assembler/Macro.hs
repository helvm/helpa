module HelVM.HelPA.Assembler.Macro where

import           HelVM.HelPA.Assembler.Value

data Macro a =
    Micro a
  | Call Header
  | Def Header [Macro a]
  deriving stock (Eq , Read , Show)

data Header = Header [IntegerValue] Text
  deriving stock (Eq , Read , Show)
