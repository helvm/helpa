module HelVM.HelPA.Assembler.AssemblyOptions where

import           HelVM.HelPA.Assembler.TokenType

data AssemblyOptions = AssemblyOptions
  { tokenType          :: !TokenType
  , debug              :: !Bool
  , startOfInstruction :: !Bool
  , endOfLine          :: !Bool
  }
