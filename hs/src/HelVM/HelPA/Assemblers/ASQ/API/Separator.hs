module HelVM.HelPA.Assemblers.ASQ.API.Separator where

data Separator = EOL | Space
  deriving stock (Eq , Read , Show)

separators :: [Separator]
separators = [EOL , Space]

defaultSeparator :: Separator
defaultSeparator = Space

parseSeparator :: String -> Separator
parseSeparator raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Separator. Valid separators are : " <> show separators
