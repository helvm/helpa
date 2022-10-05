module Main where

import           AppOptions
import           Lang

import qualified HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions as ASQ
import qualified HelVM.HelPA.Assemblers.ASQ.Assembler           as ASQ

import qualified HelVM.HelPA.Assemblers.EAS.Assembler           as EAS

import qualified HelVM.HelPA.Assemblers.WSA.Assembler           as WSA
import qualified HelVM.HelPA.Assemblers.WSA.AssemblyOptions     as WSA

import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.HelPA.Assembler.IO.BusinessIO


import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           Options.Applicative

main :: IO ()
main = run =<< customExecParser p opts where
  p = prefs disambiguate
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelPA: Heavenly Esoteric Little Portable Assembler to Esoteric Languages implemented in Haskell"
     <> progDesc "Asseble esoteric programs" )

run :: AppOptions -> IO ()
run (AppOptions lang version separator questionMark addOutLabel tokenType debug startOfInstruction endOfLine printLogs dir file) =
  putTextLn =<< controlTToIO printLogs (assemble lang asqOptions wsaOptions sourcePath) where  --FIXME Bug in relude doc for putTextLn
    asqOptions   = ASQ.AssemblyOptions {version=version, questionMark=questionMark, separator=separator , addOutLabel=addOutLabel}
    wsaOptions   = WSA.AssemblyOptions {tokenType=tokenType, debug=debug , startOfInstruction=startOfInstruction , endOfLine=endOfLine}
    sourcePath   = SourcePath {dirPath = dir , filePath = file}

assemble :: BIO m => Lang -> ASQ.AssemblyOptions -> WSA.AssemblyOptions -> SourcePath -> m Text
assemble ASQ      asqOptions _          = ASQ.assembleFile asqOptions
assemble EAS      _          _          = EAS.assembleFile
assemble (WSA  _) _          wsaOptions = WSA.assembleFile wsaOptions
assemble HAPAPL   _          _          = hapapl

hapapl :: BIO m => SourcePath -> m Text
hapapl _ = liftError "HAPAPL is not supported now"
