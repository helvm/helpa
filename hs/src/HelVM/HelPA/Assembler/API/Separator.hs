module HelVM.HelPA.Assembler.API.Separator where

import           HelVM.HelIO.SwitchEnum

defaultSeparator :: Separator
defaultSeparator = defaultEnum

separators :: [Separator]
separators = bothEnums

data Separator = Space | EOL
  deriving stock (Bounded , Enum , Eq , Read , Show)
