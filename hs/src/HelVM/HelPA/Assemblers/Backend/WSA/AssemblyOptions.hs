module HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptions where

import           HelVM.HelPA.Assemblers.Backend.WSA.API.TokenType

data AssemblyOptions = AssemblyOptions
  { tokenType          :: !TokenType
  , debug              :: !Bool -- Generate debug instructions
  , startOfInstruction :: !Bool -- Add `E` instruction
  , endOfLine          :: !Bool -- Add EOL
  }
