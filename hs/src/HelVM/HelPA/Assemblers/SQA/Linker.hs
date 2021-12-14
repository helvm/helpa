module HelVM.HelPA.Assemblers.SQA.Linker (
  linkApp
) where

import           HelVM.HelPA.Assemblers.SQA.AsmParser
import           HelVM.HelPA.Assemblers.SQA.Instruction

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

linkApp :: BIO m => SourcePath -> m InstructionList
linkApp sourcePath = parseAssemblyText =<< wReadFile (filePath sourcePath)
