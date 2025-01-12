module HelVM.HelPA.Assemblers.Frontend.FBF.FileExtra (
  buildAbsolutePathToBfFile,
  buildAbsolutePathToIlFile,
  libDir,
  appDir,
  langDir,
  dir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToBfFile :: FilePath -> FilePath
buildAbsolutePathToBfFile fileName = lang </> "bf" </> fileName <.> "bf"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = lang </> "il" </> fileName <.> "il"

libDir :: FilePath
libDir = langDir </> "libs"

appDir :: FilePath
appDir = langDir </> "apps"

langDir :: FilePath
langDir = dir </> lang

dir :: FilePath
dir = "examples"

lang :: FilePath
lang = "fbf"
