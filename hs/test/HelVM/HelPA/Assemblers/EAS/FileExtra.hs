module HelVM.HelPA.Assemblers.EAS.FileExtra (
  buildAbsolutePathToEtaFile,
  buildAbsolutePathToEasFile,
  buildEasFileName,
  easDir
) where

import           System.FilePath.Posix

buildAbsolutePathToEtaFile :: FilePath -> FilePath
buildAbsolutePathToEtaFile fileName = lang </> "eta" </> fileName <.> "eta"

buildAbsolutePathToEasFile :: FilePath -> FilePath
buildAbsolutePathToEasFile fileName = easDir </> buildEasFileName fileName

buildEasFileName :: FilePath -> FilePath
buildEasFileName fileName = fileName <.> lang

easDir :: FilePath
easDir = dir </> "eas"

dir :: FilePath
dir = "examples"

lang :: FilePath
lang = "eas"
