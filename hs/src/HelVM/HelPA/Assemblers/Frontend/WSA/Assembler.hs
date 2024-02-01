module HelVM.HelPA.Assemblers.Frontend.WSA.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptions
import           HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.Frontend.WSA.Linker

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

assembleFile :: BIO m => AssemblyOptions -> SourcePath -> m Text
assembleFile options sourcePath = reduceAndGenerateCode options =<< linkApp sourcePath

assembleText :: MonadSafe m => AssemblyOptions -> Text -> m Text
assembleText options code = reduceAndGenerateCode options =<< parseAssemblyText code
