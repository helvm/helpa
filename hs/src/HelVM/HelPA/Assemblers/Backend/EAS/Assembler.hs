module HelVM.HelPA.Assemblers.Backend.EAS.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.Backend.EAS.AsmParser
import           HelVM.HelPA.Assemblers.Backend.EAS.CodeGenerator
import           HelVM.HelPA.Assemblers.Backend.EAS.Linker

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

assembleFile :: BIO m => SourcePath -> m Text
assembleFile sourcePath = reduceAndGenerateCode =<< linkApp sourcePath

assembleText :: MonadSafe m => Text -> m Text
assembleText code = reduceAndGenerateCode =<< parseAssemblyText code
