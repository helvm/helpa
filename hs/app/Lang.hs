module Lang where

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

--langs :: [Lang]
--langs = [HAPAPL , ASQ, EAS , WSA]

data Lang = HAPAPL | ASQ | EAS | WSA TokenType
  deriving stock (Eq , Read , Show)
