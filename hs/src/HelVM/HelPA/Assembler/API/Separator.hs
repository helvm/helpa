module HelVM.HelPA.Assembler.API.Separator where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

parseSeparator :: String -> Separator
parseSeparator raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "' is not valid Separator. Valid separators are : " <> show separators

defaultSeparator :: Separator
defaultSeparator = defaultEnum

separators :: [Separator]
separators = bothEnums

data Separator = Space | EOL
  deriving stock (Bounded , Enum , Eq , Read , Show)
