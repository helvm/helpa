{-# Language DataKinds          #-}
{-# Language ExplicitNamespaces #-}

module AppOptions where

import Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to assembly " <> show langs)
                   <> value (show HAPAPL)
                   <> showDefault
                   )
  <*> switch       (  long    "debug"
                   <> short   'D'
                   <> help    "Debug (only for WS)"
                   <> showDefault
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE"
                   <> help   "File to assemble"
                   <> showDefault
                   )
  <*> argument str (  metavar "DIR"
                   <> help   "Directory with library"
                   <> value "."
                   <> showDefault
                   )
                     

data AppOptions = AppOptions
  { lang         :: String --Lang
  , debug        :: Bool
  , file         :: String
  , dir          :: String
  }

----

data Lang = HAPAPL | EAS | WSA
  deriving (Eq, Read, Show)

langs :: [Lang]
langs = [HAPAPL, EAS, WSA]

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error $ "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs
