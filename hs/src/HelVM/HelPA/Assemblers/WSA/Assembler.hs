module HelVM.HelPA.Assemblers.WSA.Assembler (
  assembleFile,
  assembleText
) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.CodeGenerator
import HelVM.HelPA.Assemblers.WSA.Linker

import HelVM.HelPA.Assembler.API
import HelVM.HelPA.Assembler.AssemblyOptions

import HelVM.HelPA.Assembler.IO.WrapperIO

import HelVM.Common.Safe
import HelVM.Common.SafeMonadT

assembleFile :: WrapperIO m => SourcePath -> AssemblyOptions -> SafeFail m Text
assembleFile sourcePath = runExceptT . exceptTAssembleFile sourcePath

exceptTAssembleFile :: WrapperIO m => SourcePath -> AssemblyOptions -> SafeMonadT m Text
exceptTAssembleFile sourcePath options = (hoistSafe . reduceAndGenerateCode options) =<< exceptTLinkApp sourcePath

assembleText :: Text -> AssemblyOptions -> Safe Text
assembleText code options = reduceAndGenerateCode options =<< parseAssemblyText code
