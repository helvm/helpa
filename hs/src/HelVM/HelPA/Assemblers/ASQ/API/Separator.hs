module HelVM.HelPA.Assemblers.ASQ.API.Separator where

import           HelVM.HelPA.Assembler.API.SwitchEnum

data Separator = Space | EOL
  deriving stock (Bounded , Enum , Eq , Read , Show)

separators :: [Separator]
separators = bothEnums

defaultSeparator :: Separator
defaultSeparator = defaultEnum

parseSeparator :: String -> Separator
parseSeparator raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Separator. Valid separators are : " <> show separators
