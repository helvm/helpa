module HelVM.HelPA.Assemblers.SQ.Assembler where

import HelVM.HelPA.Assemblers.SQ.AsmParser
import HelVM.HelPA.Assemblers.SQ.CodeGenerator
import HelVM.HelPA.Assemblers.SQ.Linker
import HelVM.HelPA.Assemblers.SQ.Reducer

import Control.Monad.Except

import qualified Data.Text as T

assemblyIO :: String -> String -> IO (Either String String)
assemblyIO dirName fileName = runExceptT $ assembly dirName fileName

assembly :: String -> String -> ExceptT String IO String
assembly dirName fileName = generateCode . reduce <$> link dirName fileName

singleAssembly :: T.Text -> Either String String
singleAssembly t = generateCode . reduce <$> parseAssembler t
