{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           HelVM.HelPA.Assembler.API

import           HelVM.HelPA.Assembler.IO.BusinessIO
import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Separator
import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import qualified HelVM.HelPA.Assemblers.ASQ.Assembler        as ASQ
import qualified HelVM.HelPA.Assemblers.ASQ.AssemblyOptions  as ASQ

import qualified HelVM.HelPA.Assemblers.EAS.Assembler        as EAS

import qualified HelVM.HelPA.Assemblers.WSA.Assembler        as WSA
import qualified HelVM.HelPA.Assemblers.WSA.AssemblyOptions  as WSA

import           HelVM.Common.Safe

import           AppOptions

import           Options.Applicative

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelPA: Heavenly Esoteric Little Portable Assembler to Esoteric Languages implemented in Haskell"
     <> progDesc "" )

run :: AppOptions -> IO ()
run AppOptions{lang , questionMark , separator , tokenType , debug , startOfInstruction , endOfLine , dir , file} = do
  putTextLn =<< exceptTToIO (eval lang' asqOptions wsaOptions sourcePath) where  --FIXME Bug in relude doc
    asqOptions = ASQ.AssemblyOptions {questionMark=questionMark', separator=separator'}
    wsaOptions = WSA.AssemblyOptions {tokenType=tokenType', debug=debug , startOfInstruction=startOfInstruction , endOfLine=endOfLine}
    sourcePath = SourcePath {dirPath = dir , filePath = file}
    questionMark' = parseQuestionMark questionMark
    separator'    = parseSeparator separator
    tokenType'    = parseTokenType tokenType
    lang'         = computeLang lang

eval :: BIO m => Lang -> ASQ.AssemblyOptions -> WSA.AssemblyOptions -> SourcePath -> m Text
eval ASQ    asqOptions _          path = ASQ.assembleFile path asqOptions
eval EAS    _          _          path = EAS.assembleFile path
eval WSA    _          wsaOptions path = WSA.assembleFile path wsaOptions
eval HAPAPL _          _          _    = hapapl

hapapl :: BIO m => m Text
hapapl = liftError "HAPAPL is not supported now"
