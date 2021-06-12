{-# Language NamedFieldPuns   #-}
module Main where

import HelVM.HelPA.Assembler.API

import HelVM.HelPA.Assembler.AssemblyOptions
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
run AppOptions{lang, tokenType, debug, startOfInstruction, endOfLine, dir, file} = do
  eval lang' assemblerOptions sourcePath where
    assemblerOptions = (AssemblyOptions {tokenType=tokenType', debug=debug, startOfInstruction=startOfInstruction, endOfLine=endOfLine})
    sourcePath = (SourcePath {dirPath = dir, filePath = file})
    tokenType' = parseTokenType tokenType
    lang' = computeLang lang

eval :: Lang -> AssemblyOptions -> SourcePath -> IO ()
eval EAS    _       = putExcept . EAS.assembleFile
eval WSA    options = putExcept . flip WSA.assembleFile options
eval HAPAPL _       = hapapl

putExcept :: SafeFail IO Text -> IO ()
putExcept io = putTextLn . unsafe =<< io --FIXME Bug in relude doc

hapapl :: SourcePath -> IO ()
hapapl _ = putStrLn "HAPAPL is not supported now"
