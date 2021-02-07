module HelVM.HelPA.Assemblers.EAS.Assembler (
  assembleFile,
  assembleText
) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.Linker

import HelVM.HelPA.Common.API

assembleFile :: SourcePath -> ParsedIO String
assembleFile = runExceptT . exceptTAssembleFile

exceptTAssembleFile :: SourcePath -> ParsedExceptT String
exceptTAssembleFile sourcePath = reduceAndGenerateCode <$> exceptTLinkApp sourcePath

assembleText :: Text -> Parsed String
assembleText code = reduceAndGenerateCode <$> parseAssemblyText code
