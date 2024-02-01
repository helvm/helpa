module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.AsmParser
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducer

import           HelVM.HelPA.Assemblers.Backend.ASQ.API.AssemblyOptions
import           HelVM.HelPA.Assemblers.Backend.ASQ.Util.CodeGenerator

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

assembleFile :: BIO m => AssemblyOptions -> SourcePath -> m Text
assembleFile options sourcePath = assembleText options =<< wReadFile (filePath sourcePath)

assembleText :: MonadSafe m => AssemblyOptions -> Text -> m Text
assembleText options code = reduceAndGenerateCode options =<< parseAssemblyText code

reduceAndGenerateCode :: MonadSafe m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (separator options) <$> reduce (addOutLabel options) (questionMark options) il
