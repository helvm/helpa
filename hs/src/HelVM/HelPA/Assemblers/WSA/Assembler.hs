module HelVM.HelPA.Assemblers.WSA.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.WSA.AsmParser
import           HelVM.HelPA.Assemblers.WSA.AssemblyOptions
import           HelVM.HelPA.Assemblers.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.WSA.Linker

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

assembleFile :: BIO m => AssemblyOptions -> SourcePath -> m Text
assembleFile options sourcePath = reduceAndGenerateCode options =<< linkApp sourcePath

assembleText :: MonadSafe m => AssemblyOptions -> Text -> m Text
assembleText options code = reduceAndGenerateCode options =<< parseAssemblyText code
