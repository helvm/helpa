module HelVM.HelPA.Assemblers.WSA.Assembler (
  assembleFile,
  assembleText
) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.CodeGenerator
import HelVM.HelPA.Assemblers.WSA.Linker

import HelVM.HelPA.Common.API
import HelVM.HelPA.Common.AssemblyOptions

assembleFile :: SourcePath -> AssemblyOptions -> ParsedIO String
assembleFile sourcePath = runExceptT . exceptTAssembleFile sourcePath

exceptTAssembleFile :: SourcePath -> AssemblyOptions -> ParsedExceptT String
exceptTAssembleFile sourcePath options = reduceAndGenerateCode options <$> exceptTLinkApp sourcePath

assembleText :: Text -> AssemblyOptions -> Parsed String
assembleText code options = reduceAndGenerateCode options <$> parseAssemblyText code
