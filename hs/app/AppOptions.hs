{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

module AppOptions where

import           Lang

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
  <*> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
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
  , eolSeparator               :: !EolSeparator
  , nextAddressForQuestionMark :: !NextAddressForQuestionMark
  , addOutLabel                :: !AddOutLabel
  , tokenType                  :: !String --TokenType
  , debug                      :: !Debug
  , startOfInstruction         :: !StartOfInstruction
  , endOfLine                  :: !StartOfInstruction
  , printLogs                  :: !PrintLogs
  , file                       :: !String
  , dir                        :: !String
  }

type EolSeparator               = Bool
type NextAddressForQuestionMark = Bool
type AddOutLabel                = Bool
type Debug                      = Bool
type StartOfInstruction         = Bool
type EndOfLine                  = Bool
type PrintLogs                  = Bool
