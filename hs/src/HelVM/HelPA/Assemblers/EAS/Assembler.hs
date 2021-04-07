module HelVM.HelPA.Assemblers.EAS.Assembler where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.Linker
import HelVM.HelPA.Assemblers.EAS.Reducer

assemblyIO :: String -> String -> IO (Either String String)
assemblyIO dirPath filePath = runExceptT $ assembly dirPath filePath

assembly :: String -> String -> ExceptT String IO String
assembly dirPath filePath = generateCode . reduce <$> link dirPath filePath

singleAssembly :: Text -> Either String String
singleAssembly t = generateCode . reduce <$> parseAssembler t
