module HelVM.HelPA.Assemblers.ASQ.API.Version where

import           HelVM.HelIO.SwitchEnum

defaultVersion :: Version
defaultVersion = defaultEnum

versions :: [Version]
versions = bothEnums

data Version = Eigenratios | EsoLangs
  deriving stock (Bounded , Enum , Eq , Read , Show)
