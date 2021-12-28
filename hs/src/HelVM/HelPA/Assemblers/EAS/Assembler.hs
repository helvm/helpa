module HelVM.HelPA.Assemblers.EAS.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.EAS.AsmParser
import           HelVM.HelPA.Assemblers.EAS.CodeGenerator
import           HelVM.HelPA.Assemblers.EAS.Linker

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Control.Safe

assembleFile :: BIO m => SourcePath -> m Text
assembleFile sourcePath = reduceAndGenerateCode =<< linkApp sourcePath

assembleText :: MonadSafe m => Text -> m Text
assembleText code = reduceAndGenerateCode =<< parseAssemblyText code
