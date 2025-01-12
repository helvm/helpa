module HelVM.HelPA.Assemblers.Frontend.HPL.FileExtra (
  buildAbsolutePathToIlFile,
  libDir,
  appDir,
  app2Dir,
  dir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = lang </> "il" </> fileName <.> "il"

libDir :: FilePath
libDir = langDir </> "stdlib"

appDir :: FilePath
appDir = langDir </> "turorial"

app2Dir :: FilePath
app2Dir = langDir </> "programs"

langDir :: FilePath
langDir = dir </> lang

dir :: FilePath
dir = "examples"

lang :: FilePath
lang = "hpl"
