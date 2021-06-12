module HelVM.HelPA.Assemblers.WSA.Linker where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assembler.API
import HelVM.HelPA.Assembler.Value

import HelVM.HelPA.Assembler.IO.WrapperIO

import HelVM.Common.Safe
import HelVM.Common.SafeMonadT

linkLib :: WrapperIO m => SourcePath -> SafeFail m InstructionList
linkLib = runExceptT . expectTLinkLib

linkApp :: WrapperIO m => SourcePath -> SafeFail m InstructionList
linkApp = runExceptT . exceptTLinkApp

expectTLinkLib :: WrapperIO m => SourcePath -> SafeMonadT m InstructionList
expectTLinkLib = exceptTLinkApp . absolutePath

exceptTLinkApp :: WrapperIO m => SourcePath -> SafeMonadT m InstructionList
exceptTLinkApp path = (exceptTIncludeLibs (dirPath path) =<<) $ ExceptT $ parseAssemblyText <$> wReadFile (filePath path)

exceptTIncludeLibs :: WrapperIO m => String -> InstructionList -> SafeMonadT m InstructionList
exceptTIncludeLibs dir il = sortBlocks <$> mapM (exceptTIncludeLib dir) il

sortBlocks :: [Block InstructionList] -> InstructionList
sortBlocks list = unwrap =<< (filter isNormal list <> filter isIncluded list)

exceptTIncludeLib :: WrapperIO m => String -> Instruction -> SafeMonadT m (Block InstructionList)
exceptTIncludeLib dir (Include libName) = Included <$> expectTLinkLib (SourcePath {dirPath = dir, filePath = unwrapIdentifier libName <> ".wsa"})
exceptTIncludeLib _ i = pure $ Normal [i]

unwrap :: Block a -> a
unwrap (Normal a) = a
unwrap (Included a) = a

isNormal :: Block a -> Bool
isNormal (Normal   _) = True
isNormal (Included _) = False

isIncluded :: Block a -> Bool
isIncluded (Normal   _) = False
isIncluded (Included _) = True

data Block a = Normal a | Included a
