module HelVM.HelPA.Assemblers.EAS.Assembler where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.Linker
import HelVM.HelPA.Assemblers.EAS.Reducer

import Control.Monad.Except

import qualified Data.Text as T

assemblyIO :: String -> String -> IO (Either String String)
assemblyIO dirName fileName = runExceptT $ assembly dirName fileName

assembly :: String -> String -> ExceptT String IO String
assembly dirName fileName = generateCode . reduce <$> link dirName fileName

singleAssembly :: T.Text -> Either String String
singleAssembly t = generateCode . reduce <$> parseAssembler t
