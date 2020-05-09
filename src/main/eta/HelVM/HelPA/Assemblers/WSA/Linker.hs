module HelVM.HelPA.Assemblers.WSA.Linker where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction

import Control.Monad.Except

import qualified Data.Text.IO as T

linkLibIO :: String -> String -> IO (Either String InstructionList)
linkLibIO dirPath fileName = runExceptT $ linkLib dirPath fileName

linkIO :: String -> String -> IO (Either String InstructionList)
linkIO dirPath filePath = runExceptT $ link dirPath filePath

linkLib :: String -> String -> ExceptT String IO InstructionList
linkLib dirPath fileName = link dirPath $ dirPath ++ "/" ++ fileName

link :: String -> String -> ExceptT String IO InstructionList
link dirPath filePath = (includeLibs dirPath =<<) $ ExceptT $ parseAssembler <$> T.readFile filePath

includeLibs :: String -> InstructionList -> ExceptT String IO InstructionList
includeLibs dirPath il = sortBlocks <$> mapM (includeLib dirPath) il

sortBlocks :: [Block InstructionList] -> InstructionList
sortBlocks list = unwrap =<< (filter isNormal list ++ filter isIncluded list)

includeLib :: String -> Instruction -> ExceptT String IO (Block InstructionList)
includeLib dirPath (Include libName) = Included <$> linkLib dirPath (libName ++ ".wsa")
includeLib _ i = pure $ Normal [i]

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
