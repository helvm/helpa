module HelVM.HelPA.Assemblers.SQA.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.SQA.AsmParser
import           HelVM.HelPA.Assemblers.SQA.CodeGenerator
import           HelVM.HelPA.Assemblers.SQA.Linker

import           HelVM.HelPA.Assemblers.ASQ.AssemblyOptions

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Safe

assembleFile :: BIO m => SourcePath -> AssemblyOptions -> m Text
assembleFile sourcePath options = reduceAndGenerateCode options =<< linkApp sourcePath

assembleText :: MonadSafeError m => Text -> AssemblyOptions -> m Text
assembleText code options = reduceAndGenerateCode options =<< parseAssemblyText code
