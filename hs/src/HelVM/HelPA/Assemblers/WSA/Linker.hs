module HelVM.HelPA.Assemblers.WSA.Linker where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Common.API

linkLib :: SourcePath -> ParsedIO InstructionList
linkLib = runExceptT . expectTLinkLib

linkApp :: SourcePath -> ParsedIO InstructionList
linkApp = runExceptT . exceptTLinkApp

expectTLinkLib :: SourcePath -> ParsedExceptT InstructionList
expectTLinkLib = exceptTLinkApp . absolutePath

exceptTLinkApp :: SourcePath -> ParsedExceptT InstructionList
exceptTLinkApp path = (exceptTIncludeLibs (dirPath path) =<<) $ ExceptT $ parseAssemblyText <$> readFileText (filePath path)

exceptTIncludeLibs :: String -> InstructionList -> ParsedExceptT InstructionList
exceptTIncludeLibs dir il = sortBlocks <$> mapM (exceptTIncludeLib dir) il

sortBlocks :: [Block InstructionList] -> InstructionList
sortBlocks list = unwrap =<< (filter isNormal list <> filter isIncluded list)

exceptTIncludeLib :: String -> Instruction -> ParsedExceptT (Block InstructionList)
exceptTIncludeLib dir (Include libName) = Included <$> expectTLinkLib (SourcePath {dirPath = dir, filePath = libName <> ".wsa"})
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
