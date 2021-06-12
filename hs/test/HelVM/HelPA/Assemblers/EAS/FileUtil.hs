module HelVM.HelPA.Assemblers.EAS.FileUtil (
  buildAbsolutePathToEtaFile,
  buildAbsolutePathToEasFile,
  buildEasFileName,
  easDir
) where

import System.FilePath.Posix

buildAbsolutePathToEtaFile :: FilePath -> FilePath
buildAbsolutePathToEtaFile fileName = dir </> "eta" </> fileName <.> "eta"

buildAbsolutePathToEasFile :: FilePath -> FilePath
buildAbsolutePathToEasFile fileName = easDir </> buildEasFileName fileName

buildEasFileName :: FilePath -> FilePath
buildEasFileName fileName = fileName <.> "eas"

easDir :: FilePath
easDir = dir </> "eas"

dir :: FilePath
dir = "examples"
