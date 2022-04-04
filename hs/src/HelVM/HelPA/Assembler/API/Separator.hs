module HelVM.HelPA.Assembler.API.Separator where

import           HelVM.HelPA.Assembler.API.SwitchEnum

parseSeparator :: String -> Separator
parseSeparator raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Separator. Valid separators are : " <> show separators

defaultSeparator :: Separator
defaultSeparator = defaultEnum

separators :: [Separator]
separators = bothEnums

data Separator = Space | EOL
  deriving stock (Bounded , Enum , Eq , Read , Show)
