module HelVM.HelPA.Assemblers.EAS.FileUtil where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.Instruction
import HelVM.HelPA.Assemblers.EAS.Linker
import HelVM.HelPA.Assemblers.Util

dir = "examples/"

easDir = dir <> "eas/"

buildEtaFileName fileName = fileName <> ".eas"

buildAbsoluteEtaFileName fileName = easDir <> buildEtaFileName fileName

----

assemblyFile :: String -> IO (Either String String)
assemblyFile fileName = assemblyIO easDir $ buildAbsoluteEtaFileName fileName

parseFromFile :: String -> IO InstructionList
parseFromFile fileName = toIO . parseAssembler =<< readFileText (buildAbsoluteEtaFileName fileName)

linkFile :: String -> IO InstructionList
linkFile fileName = toIO =<< linkLibIO "examples/eas/" (buildEtaFileName fileName)

readEtaFile :: String -> IO String
readEtaFile fileName = readFile $ dir <> "eta/" <> fileName <> ".eta"
