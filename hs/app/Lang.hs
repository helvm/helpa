module Lang where

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a) = a
  valid Nothing  = error $ "'" <> toText raw <> "lang is not valid lang. Valid langs are : " <> show langs

langs :: [Lang]
langs = [HAPAPL , ASQ, EAS , WSA]

data Lang = HAPAPL | ASQ | EAS | WSA
  deriving stock (Bounded , Enum , Eq , Read , Show)
