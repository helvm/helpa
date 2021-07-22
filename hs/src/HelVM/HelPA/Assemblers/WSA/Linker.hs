module HelVM.HelPA.Assemblers.WSA.Linker where

import           HelVM.HelPA.Assemblers.WSA.AsmParser
import           HelVM.HelPA.Assemblers.WSA.Instruction

import           HelVM.HelPA.Assembler.API
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelPA.Assembler.IO.BusinessIO

import           Control.Type.Operator

linkLib :: BIO m => SourcePath -> m InstructionList
linkLib = expectTLinkLib

linkApp :: BIO m => SourcePath -> m InstructionList
linkApp = exceptTLinkApp

expectTLinkLib :: BIO m => SourcePath -> m InstructionList
expectTLinkLib = exceptTLinkApp . absolutePath

exceptTLinkApp :: BIO m => SourcePath -> m InstructionList
exceptTLinkApp path = (exceptTIncludeLibs (dirPath path) =<<) $ parseAssemblyText =<< wReadFile (filePath path)

exceptTIncludeLibs :: BIO m => FilePath -> InstructionList -> m InstructionList
exceptTIncludeLibs dir il = sortBlocks <$> mapM (exceptTIncludeLib dir) il

sortBlocks :: [Block InstructionList] -> InstructionList
sortBlocks list = unwrap =<< (filter isNormal list <> filter isIncluded list) --FIXME groupBy ?

exceptTIncludeLib :: BIO m => FilePath -> Instruction -> m $ Block InstructionList
exceptTIncludeLib dir (Include libName) = Included <$> expectTLinkLib (SourcePath {dirPath = dir , filePath = unwrapIdentifier libName <> ".wsa"})
exceptTIncludeLib _ i                   = pure $ Normal [i]

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
