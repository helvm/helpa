module HelVM.HelPA.Assemblers.WSA.AssemblyOptions where

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

data AssemblyOptions = AssemblyOptions
  { tokenType          :: !TokenType
  , debug              :: !Bool -- Generate debug instructions
  , startOfInstruction :: !Bool -- Add `E` instruction
  , endOfLine          :: !Bool -- Add EOL
  }
