module HelVM.HelPA.Assemblers.EAS.Assembler (
  assembleFile,
  assembleText
) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.Linker

import HelVM.HelPA.Assembler.API
import HelVM.HelPA.Assembler.IO.WrapperIO

import HelVM.Common.Safe
import HelVM.Common.SafeMonadT

assembleFile :: WrapperIO m => SourcePath -> SafeFail m Text
assembleFile = runExceptT . exceptTAssembleFile

exceptTAssembleFile :: WrapperIO m => SourcePath -> SafeMonadT m Text
exceptTAssembleFile sourcePath = exceptTAssembleFile' =<< exceptTLinkApp sourcePath where
  exceptTAssembleFile' code = hoistSafe $ reduceAndGenerateCode code

assembleText :: Text -> Safe Text
assembleText code = reduceAndGenerateCode =<< parseAssemblyText code
