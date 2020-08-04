{-# Language DataKinds          #-}
{-# Language ExplicitNamespaces #-}

module AppOptions where

import Options.Applicative

import Text.Read

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to assembly " ++ show langs)
                   <> value (show HAPAPL)
                   <> showDefault
                   )
  <*> argument str (  metavar "DIR")
  <*> argument str (  metavar "FILE")
                     

data AppOptions = AppOptions
  { lang         :: String --Lang
  , dir          :: String
  , file         :: String
  }

----

data Lang = HAPAPL | EAS
  deriving (Eq, Read, Show)

langs :: [Lang]
langs = [HAPAPL, EAS]

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error ("Lang '" ++ raw ++ "' is not valid lang. Valid langs are : " ++ show langs)
