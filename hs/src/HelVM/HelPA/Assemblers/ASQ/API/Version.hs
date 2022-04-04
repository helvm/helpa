module HelVM.HelPA.Assemblers.ASQ.API.Version where

parseVersion :: String -> Version
parseVersion raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Version. Valid version are : " <> show versions

defaultVersion :: Version
defaultVersion = Eigenratios

versions :: [Version]
versions = [Eigenratios , EsoLangs]

data Version = Eigenratios | EsoLangs
  deriving stock (Bounded , Enum , Eq , Read , Show)
