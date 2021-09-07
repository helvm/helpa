module HelVM.HelPA.Assemblers.ASQ.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.ASQ.AsmParser
import           HelVM.HelPA.Assemblers.ASQ.AssemblyOptions
import           HelVM.HelPA.Assemblers.ASQ.CodeGenerator
import           HelVM.HelPA.Assemblers.ASQ.Linker

import           HelVM.HelPA.Assembler.API
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Safe

assembleFile :: BIO m => SourcePath -> AssemblyOptions -> m Text
assembleFile sourcePath options = reduceAndGenerateCode options =<< linkApp sourcePath

assembleText :: MonadSafeError m => Text -> AssemblyOptions -> m Text
assembleText code options = reduceAndGenerateCode options =<< parseAssemblyText code
