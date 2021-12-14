{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

module AppOptions where

import           HelVM.HelPA.Assemblers.ASQ.API.Version
import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to exceptTAssembleFile " <> show langs)
                   <> value (show HAPAPL)
                   <> showDefault
                   )
  <*> strOption    (  long    "version"
                   <> short   'V'
                   <> help    "Version of Assembler (only for SQ)"
                   <> value (show defaultVersion)
                   <> showDefault
                   )
  <*> switch       (  long    "eolSeparator"
                   <> short   'S'
                   <> help    "EOL separator (only for SQ)"
                   <> showDefault
                   )
  <*> switch       (  long    "nextAddressForQuestionMark"
                   <> short   'Q'
                   <> help    "Type of Question Mark (only for SQ)"
                   <> showDefault
                   )
  <*> switch       (  long    "addOutLabel"
                   <> help    "Add OUT label (only for SQ)"
                   <> showDefault
                   )
  <*> strOption    (  long    "tokenType"
                   <> short   'T'
                   <> help    "Type of Tokens (only for WS)"
                   <> value (show defaultTokenType)
                   <> showDefault
                   )
  <*> switch       (  long    "debug"
                   <> short   'D'
                   <> help    "Debug (only for WS)"
                   <> showDefault
                   )
  <*> switch       (  long    "startOfInstruction"
                   <> short   'S'
                   <> help    "StartOfInstruction (only for WS)"
                   <> showDefault
                   )
  <*> switch       (  long    "endOfLine"
                   <> short   'E'
                   <> help    "EndOfLine (only for WS)"
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
  { lang                       :: !String --Lang
  , version                    :: !String --Version
  , eolSeparator               :: !Bool
  , nextAddressForQuestionMark :: !Bool
  , addOutLabel                :: !Bool
  , tokenType                  :: !String --TokenType
  , debug                      :: !Bool
  , startOfInstruction         :: !Bool
  , endOfLine                  :: !Bool
  , file                       :: !String
  , dir                        :: !String
  }

----

data Lang = HAPAPL | ASQ | EAS | WSA
  deriving stock (Eq , Read , Show)

langs :: [Lang]
langs = [HAPAPL , ASQ, EAS , WSA]

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a) = a
  valid Nothing  = error $ "'" <> toText raw <> "lang ' is not valid lang. Valid langs are : " <> show langs
