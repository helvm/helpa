module HelVM.HelPA.Assemblers.Frontend.SblAsm.Assembler where

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

assembleFile :: BIO m => SourcePath -> m Text
assembleFile sourcePath = assembleText =<< wReadFile (filePath sourcePath)

assembleText :: MonadSafe m => Text -> m Text
assembleText = pure
