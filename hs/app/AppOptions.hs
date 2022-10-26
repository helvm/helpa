{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

module AppOptions where

import           Lang

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Version

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import           HelVM.HelPA.Assembler.API.Separator

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> option auto  (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to exceptTAssembleFile " <> langHelp)
                   <> value    HAPAPL
                   <> showDefault
                   )
--  <$> subparser
--    (
--         command "HAPAPL" )
--      <> command "ASQ" (info addCommand ( progDesc "Add a file to the repository" ))
--      <> command "EAS" (info addCommand ( progDesc "Add a file to the repository" ))
--      <> command "WSA" (info commitCommand ( progDesc "Record changes to the repository" ))
--    )
  <*> option auto  (  long    "version"
                   <> short   'V'
                   <> help    "Version of Assembler (only for SQ)"
                   <> value    defaultVersion
                   <> showDefault
                   )
  <*> flag Space EOL
                   (  long    "eolSeparator"
                   <> short   'S'
                   <> help    "EOL separator (only for SQ)"
                   <> showDefault
                   )
  <*> flag CurrentAddress NextAddress
                   (  long    "nextAddressForQuestionMark"
                   <> short   'Q'
                   <> help    "Type of Question Mark (only for SQ)"
                   <> showDefault
                   )
  <*> switch       (  long    "addOutLabel"
                   <> help    "Add OUT label (only for SQ)"
                   <> showDefault
                   )
  <*> option auto  (  long    "tokenType"
                   <> short   'T'
                   <> help    "Type of Tokens (only for WS)"
                   <> value    defaultTokenType
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
  { lang                       :: !Lang
  , version                    :: !Version
  , eolSeparator               :: !Separator
  , nextAddressForQuestionMark :: !QuestionMark
  , addOutLabel                :: !AddOutLabel
  , tokenType                  :: !TokenType
  , debug                      :: !Debug
  , startOfInstruction         :: !StartOfInstruction
  , endOfLine                  :: !StartOfInstruction
  , printLogs                  :: !PrintLogs
  , file                       :: !String
  , dir                        :: !String
  }

type AddOutLabel                = Bool
type Debug                      = Bool
type StartOfInstruction         = Bool
type EndOfLine                  = Bool
type PrintLogs                  = Bool
