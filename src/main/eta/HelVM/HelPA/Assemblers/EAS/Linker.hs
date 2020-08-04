module HelVM.HelPA.Assemblers.EAS.Linker where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

import Control.Monad.Except

import qualified Data.Text.IO as T

linkIO :: String -> String -> IO (Either String InstructionList)
linkIO dirName fileName = runExceptT $ link dirName fileName 

link :: String -> String -> ExceptT String IO InstructionList
link dirName fileName = includeFiles $ ExceptT $ parseAssembler <$> T.readFile (dirName ++ "/" ++ fileName) where

  includeFiles :: ExceptT String IO InstructionList -> ExceptT String IO InstructionList
  includeFiles expect = loadFiles =<< expect

  loadFiles :: InstructionList -> ExceptT String IO InstructionList
  loadFiles il = concat <$> mapM loadFile il

  loadFile :: Instruction -> ExceptT String IO InstructionList
  loadFile (D libName) = link dirName libName
  loadFile i = pure [i]
