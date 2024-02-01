module HelVM.HelPA.Assemblers.Frontend.WSA.Linker where

import           HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelPA.Assembler.IO.BusinessIO

import           Control.Type.Operator

linkLib :: BIO m => SourcePath -> m InstructionList
linkLib = linkApp . absolutePath

linkApp :: BIO m => SourcePath -> m InstructionList
linkApp path = (includeLibs (dirPath path) =<<) $ parseAssemblyText =<< wReadFile (filePath path)

includeLibs :: BIO m => FilePath -> InstructionList -> m InstructionList
includeLibs dir il = sortBlocks <$> traverse (includeLib dir) il

sortBlocks :: [Block InstructionList] -> InstructionList
sortBlocks list = unwrap =<< (filter isNormal list <> filter isIncluded list) --FIXME groupBy ?

includeLib :: BIO m => FilePath -> Instruction -> m $ Block InstructionList
includeLib dir (Include libName) = Included <$> linkLib (SourcePath {dirPath = dir , filePath = unwrapIdentifier libName <> ".wsa"})
includeLib _ i                   = pure $ Normal [i]

unwrap :: Block a -> a
unwrap (Normal a)   = a
unwrap (Included a) = a

isNormal :: Block a -> Bool
isNormal (Normal   _) = True
isNormal (Included _) = False

isIncluded :: Block a -> Bool
isIncluded (Normal   _) = False
isIncluded (Included _) = True

data Block a = Normal !a | Included !a
