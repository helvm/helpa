module HelVM.HelPA.Assemblers.Backend.ASQ.API.Version where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

parseVersion :: String -> Version
parseVersion raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "' is not valid Version. Valid version are : " <> show versions

defaultVersion :: Version
defaultVersion = defaultEnum

versions :: [Version]
versions = bothEnums

data Version = Eigenratios | EsoLangs
  deriving stock (Bounded , Enum , Eq , Read , Show)
