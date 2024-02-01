module HelVM.HelPA.Assemblers.Backend.EAS.Linker (
  linkLib,
  linkApp
) where

import           HelVM.HelPA.Assemblers.Backend.EAS.AsmParser
import           HelVM.HelPA.Assemblers.Backend.EAS.Instruction

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO
import           HelVM.HelPA.Assembler.Value

linkLib :: BIO m => SourcePath -> m InstructionList
linkLib sourcePath = linkApp (absolutePath sourcePath)

linkApp :: BIO m => SourcePath -> m InstructionList
linkApp sourcePath = (includeLibs (dirPath sourcePath) =<<) $ parseAssemblyText =<< wReadFile (filePath sourcePath)

includeLibs :: BIO m => FilePath -> InstructionList -> m InstructionList
includeLibs dir il = concat <$> mapM (includeLib dir) il

includeLib :: BIO m => FilePath -> Instruction -> m InstructionList
includeLib dir (D libName) = linkLib SourcePath { dirPath = dir , filePath = unwrapIdentifier libName }
includeLib _    i          = pure [i]
