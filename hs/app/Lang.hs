module Lang where

import           HelVM.HelIO.Extra

computeLang :: String -> Lang
computeLang raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "lang is not valid lang. Valid langs are : " <> show langs

langs :: [Lang]
langs = [HAPAPL , ASQ, EAS , WSA]

data Lang = HAPAPL | ASQ | EAS | WSA
  deriving stock (Bounded , Enum , Eq , Read , Show)
