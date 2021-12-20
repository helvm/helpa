module HelVM.HelPA.Assemblers.ASQ.API.Version where

data Version = Eigenratios | EsoLangs
  deriving stock (Eq , Read , Show)

versions :: [Version]
versions = [Eigenratios , EsoLangs]

defaultVersion :: Version
defaultVersion = Eigenratios

parseVersion :: String -> Version
parseVersion raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Version. Valid version are : " <> show versions
