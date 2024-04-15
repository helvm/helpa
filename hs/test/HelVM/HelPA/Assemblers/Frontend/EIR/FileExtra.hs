module HelVM.HelPA.Assemblers.Frontend.EIR.FileExtra (
  buildAbsolutePathToWsaFile,
  buildAbsolutePathToIlFile,
  libDir,
  appDir,
  wsaDir,
  dir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToWsaFile :: FilePath -> FilePath
buildAbsolutePathToWsaFile fileName = lang </> "wsa" </> fileName <.> "wsa"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = lang </> "il" </> fileName <.> "il"

libDir :: FilePath
libDir = wsaDir </> "libs"

appDir :: FilePath
appDir = wsaDir </> "apps"

wsaDir :: FilePath
wsaDir = dir </> lang

dir :: FilePath
dir = "examples"

lang :: FilePath
lang = "eir"
