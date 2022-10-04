module Lang where

langs :: [Lang]
langs = [HAPAPL , ASQ, EAS , WSA]

data Lang = HAPAPL | ASQ | EAS | WSA
  deriving stock (Bounded , Enum , Eq , Read , Show)
