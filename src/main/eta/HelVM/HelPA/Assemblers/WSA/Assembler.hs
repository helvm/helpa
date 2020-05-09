module HelVM.HelPA.Assemblers.WSA.Assembler where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.CodeGenerator
import HelVM.HelPA.Assemblers.WSA.Linker
import HelVM.HelPA.Assemblers.WSA.Reducer

import Control.Monad.Except

import qualified Data.Text as T

assemblyIO :: Bool -> String -> String -> IO (Either String String)
assemblyIO debug dirPath filePath = runExceptT $ assembly debug dirPath filePath

assembly :: Bool -> String -> String -> ExceptT String IO String
assembly debug dirPath filePath = generateCode debug . reduce <$> link dirPath filePath

singleAssembly :: Bool -> T.Text -> Either String String
singleAssembly debug code = generateCode debug . reduce <$> parseAssembler code
