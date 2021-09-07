module HelVM.HelPA.Assemblers.ASQ.Linker (
  linkApp
) where

import           HelVM.HelPA.Assemblers.ASQ.AsmParser
import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.HelPA.Assembler.API
import           HelVM.HelPA.Assembler.IO.BusinessIO

linkApp :: BIO m => SourcePath -> m InstructionList
linkApp sourcePath = parseAssemblyText =<< wReadFile (filePath sourcePath)
