module Lang where

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

langHelp :: String
langHelp = "[HAPAPL , ASQ , EAS , WSA]"

data Lang = HAPAPL | ASQ | EAS | WSA TokenType
  deriving stock (Eq , Read , Show)
