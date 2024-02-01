module Main where

import           AppOptions
import           Lang

import qualified HelVM.HelPA.Assemblers.Backend.ASQ.API.AssemblyOptions as ASQ
import qualified HelVM.HelPA.Assemblers.Backend.ASQ.Assembler           as ASQ

import qualified HelVM.HelPA.Assemblers.Backend.EAS.Assembler           as EAS

import qualified HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptions     as WSA
import qualified HelVM.HelPA.Assemblers.Frontend.WSA.Assembler          as WSA

import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.HelPA.Assembler.IO.BusinessIO


import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           Options.Applicative

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelPA: Heavenly Esoteric Little Portable Assembler to Esoteric Languages implemented in Haskell"
     <> progDesc "" )

run :: AppOptions -> IO ()
run (AppOptions lang version separator questionMark addOutLabel tokenType debug startOfInstruction endOfLine printLogs dir file) =
  putTextLn =<< controlTToIO printLogs (eval lang asqOptions wsaOptions sourcePath) where  --FIXME Bug in relude doc for putTextLn
    asqOptions   = ASQ.AssemblyOptions {version=version, questionMark=questionMark, separator=separator , addOutLabel=addOutLabel}
    wsaOptions   = WSA.AssemblyOptions {tokenType=tokenType, debug=debug , startOfInstruction=startOfInstruction , endOfLine=endOfLine}
    sourcePath   = SourcePath {dirPath = dir , filePath = file}

eval :: BIO m => Lang -> ASQ.AssemblyOptions -> WSA.AssemblyOptions -> SourcePath -> m Text
eval ASQ    asqOptions _          = ASQ.assembleFile asqOptions
eval EAS    _          _          = EAS.assembleFile
eval WSA    _          wsaOptions = WSA.assembleFile wsaOptions
eval HAPAPL _          _          = hapapl

hapapl :: BIO m => SourcePath -> m Text
hapapl _ = liftError "HAPAPL is not supported now"
