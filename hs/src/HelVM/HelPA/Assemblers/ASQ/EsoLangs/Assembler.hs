module HelVM.HelPA.Assemblers.ASQ.EsoLangs.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.AsmParser
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducer

import           HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions
import           HelVM.HelPA.Assemblers.ASQ.Util.CodeGenerator

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Safe

assembleFile :: BIO m => AssemblyOptions -> SourcePath -> m Text
assembleFile options sourcePath = assembleText options =<< wReadFile (filePath sourcePath)

assembleText :: MonadSafeError m => AssemblyOptions -> Text -> m Text
assembleText options code = reduceAndGenerateCode options =<< parseAssemblyText code

reduceAndGenerateCode :: MonadSafeError m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (separator options) <$> reduce (addOutLabel options) (questionMark options) il
