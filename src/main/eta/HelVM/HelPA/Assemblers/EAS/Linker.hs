module HelVM.HelPA.Assemblers.EAS.Linker where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

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
includeLibs dirPath il = concat <$> mapM (includeLib dirPath) il

includeLib :: String -> Instruction -> ExceptT String IO InstructionList
includeLib dirPath (D libName) = linkLib dirPath libName
includeLib _ i = pure [i]
