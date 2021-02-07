module HelVM.HelPA.Assemblers.EAS.Linker (
  linkLib,
  linkApp,
  exceptTLinkApp
) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.API

linkLib :: SourcePath -> ParsedIO InstructionList
linkLib = runExceptT . expectTLinkLib

linkApp :: SourcePath -> ParsedIO InstructionList
linkApp = runExceptT . exceptTLinkApp

expectTLinkLib :: SourcePath -> ParsedExceptT InstructionList
expectTLinkLib = exceptTLinkApp . absolutePath

exceptTLinkApp :: SourcePath -> ParsedExceptT InstructionList
exceptTLinkApp path = (exceptTIncludeLibs (dirPath path) =<<) $ ExceptT $ parseAssemblyText <$> readFileText (filePath path)

exceptTIncludeLibs :: String -> InstructionList -> ParsedExceptT InstructionList
exceptTIncludeLibs dir il = concat <$> mapM (exceptTIncludeLib dir) il

exceptTIncludeLib :: String -> Instruction -> ParsedExceptT InstructionList
exceptTIncludeLib dir (D libName) = expectTLinkLib SourcePath { dirPath = dir, filePath = libName }
exceptTIncludeLib _ i = pure [i]
