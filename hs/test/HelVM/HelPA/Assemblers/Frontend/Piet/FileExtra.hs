module HelVM.HelPA.Assemblers.Frontend.Piet.FileExtra (
  buildAbsolutePathToPnmFile,
  buildAbsolutePathToIlFile,
  libDir,
  appDir,
  langDir,
  dir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToPnmFile :: FilePath -> FilePath
buildAbsolutePathToPnmFile fileName = lang </> "pnm" </> fileName <.> "pnm"

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
lang = "piet"
