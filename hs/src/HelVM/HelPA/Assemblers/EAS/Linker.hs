module HelVM.HelPA.Assemblers.EAS.Linker (
  linkLib,
  linkApp,
  exceptTLinkApp
) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assembler.API
import HelVM.HelPA.Assembler.IO.WrapperIO
import HelVM.HelPA.Assembler.Value

import HelVM.Common.Safe
import HelVM.Common.SafeMonadT

linkLib :: WrapperIO m => SourcePath -> SafeFail m InstructionList
linkLib = runExceptT . expectTLinkLib

linkApp :: WrapperIO m => SourcePath -> SafeFail m InstructionList
linkApp = runExceptT . exceptTLinkApp

expectTLinkLib :: WrapperIO m => SourcePath -> SafeMonadT m InstructionList
expectTLinkLib = exceptTLinkApp . absolutePath

exceptTLinkApp :: WrapperIO m => SourcePath -> SafeMonadT m InstructionList
exceptTLinkApp path = (exceptTIncludeLibs (dirPath path) =<<) $ ExceptT $ parseAssemblyText <$> wReadFile (filePath path)

exceptTIncludeLibs :: WrapperIO m => String -> InstructionList -> SafeMonadT m InstructionList
exceptTIncludeLibs dir il = concat <$> mapM (exceptTIncludeLib dir) il

exceptTIncludeLib :: WrapperIO m => String -> Instruction -> SafeMonadT m InstructionList
exceptTIncludeLib dir (D libName) = expectTLinkLib SourcePath { dirPath = dir, filePath = unwrapIdentifier libName }
exceptTIncludeLib _ i = pure [i]
