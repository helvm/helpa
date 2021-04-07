module HelVM.HelPA.Assemblers.WSA.FileUtil where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Assembler
import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.Linker
import HelVM.HelPA.Assemblers.Util

libDir = "examples/wsa/libs/"
appDir = "examples/wsa/examples/"

ext = ".wsa"

----

assembleLibFile name = toIO =<< assemblyIO False libDir (libDir <> name <> ext)
assembleFile    name = toIO =<< assemblyIO False libDir (appDir <> name <> ext)

parseLibFromFile name = toIO . parseAssembler =<< readFileText (libDir <> name <> ext)
parseFromFile    name = toIO . parseAssembler =<< readFileText (appDir <> name <> ext)

linkLibFile name = toIO =<< linkLibIO libDir (name <> ext)
linkFile    name = toIO =<< linkIO libDir (appDir <> name <> ext)
