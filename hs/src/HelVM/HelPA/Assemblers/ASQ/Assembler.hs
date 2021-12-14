module HelVM.HelPA.Assemblers.ASQ.Assembler (
  assembleFile,
  assembleText,
) where

import           HelVM.HelPA.Assemblers.ASQ.API.Version

import           HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions

import           HelVM.HelPA.Assembler.API.SourcePath
import           HelVM.HelPA.Assembler.IO.BusinessIO

import           HelVM.Common.Safe

import qualified HelVM.HelPA.Assemblers.ASQ.Eigenratios.Assembler as Eigenratios
import qualified HelVM.HelPA.Assemblers.ASQ.EsoLangs.Assembler    as EsoLangs

assembleFile :: BIO m => AssemblyOptions -> SourcePath -> m Text
assembleFile options sourcePath = assembleText (version options) options =<< wReadFile (filePath sourcePath)

assembleText :: MonadSafeError m => Version -> AssemblyOptions -> Text -> m Text
assembleText Eigenratios = Eigenratios.assembleText
assembleText EsoLangs    = EsoLangs.assembleText
