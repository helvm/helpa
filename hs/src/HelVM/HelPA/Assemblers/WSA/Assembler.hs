module HelVM.HelPA.Assemblers.WSA.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.WSA.AsmParser
import           HelVM.HelPA.Assemblers.WSA.AssemblyOptions
import           HelVM.HelPA.Assemblers.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.WSA.Linker

import           HelVM.HelPA.Assembler.API

import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Safe

assembleFile :: BIO m => SourcePath -> AssemblyOptions -> m Text
assembleFile sourcePath options = reduceAndGenerateCode options =<< linkApp sourcePath

assembleText :: MonadSafeError m => Text -> AssemblyOptions -> m Text
assembleText code options = reduceAndGenerateCode options =<< parseAssemblyText code
