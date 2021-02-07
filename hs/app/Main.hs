{-# Language NamedFieldPuns   #-}
module Main where

import HelVM.HelPA.Common.API

import HelVM.HelPA.Common.AssemblyOptions
import HelVM.HelPA.Common.TokenType

import qualified HelVM.HelPA.Assemblers.EAS.Assembler as EAS
import qualified HelVM.HelPA.Assemblers.WSA.Assembler as WSA

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

putExcept :: ParsedIO String -> IO ()
putExcept io = putStrLn . output =<< io

output :: Parsed String -> String
output (Right result) = result
output (Left message) = error $ toText message

hapapl :: SourcePath -> IO ()
hapapl _ = putStrLn "HAPAPL is not supported now"
