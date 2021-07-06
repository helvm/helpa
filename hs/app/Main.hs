{-# Language NamedFieldPuns   #-}
module Main where

import HelVM.HelPA.Assembler.API

import HelVM.HelPA.Assembler.AssemblyOptions
import HelVM.HelPA.Assembler.IO.BusinessIO
import HelVM.HelPA.Assembler.TokenType

import qualified HelVM.HelPA.Assemblers.EAS.Assembler as EAS
import qualified HelVM.HelPA.Assemblers.WSA.Assembler as WSA

import HelVM.Common.Safe

import AppOptions

import Options.Applicative

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelPA: Heavenly Esoteric Little Para Assembler to Esoteric Languages implemented in Haskell/Eta"
     <> progDesc "" )

run :: AppOptions -> IO ()
run AppOptions{lang , tokenType , debug , startOfInstruction , endOfLine , dir , file} = do
  putTextLn =<< exceptTToIO (eval lang' assemblerOptions sourcePath) where  --FIXME Bug in relude doc
    assemblerOptions = AssemblyOptions {tokenType=tokenType', debug=debug , startOfInstruction=startOfInstruction , endOfLine=endOfLine}
    sourcePath = SourcePath {dirPath = dir , filePath = file}
    tokenType' = parseTokenType tokenType
    lang' = computeLang lang



eval :: BIO m => Lang -> AssemblyOptions -> SourcePath -> m Text
eval EAS    _        path = EAS.assembleFile path
eval WSA    options  path = WSA.assembleFile path options
eval HAPAPL _        _    = hapapl

hapapl :: BIO m => m Text
hapapl = liftError "HAPAPL is not supported now"
