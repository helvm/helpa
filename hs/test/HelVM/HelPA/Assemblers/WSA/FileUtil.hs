module HelVM.HelPA.Assemblers.WSA.FileUtil (
  buildAbsolutePathToWsFile,
  buildAbsolutePathToIlFile,
  libDir,
  appDir,
  wsaDir,
  dir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToWsFile :: FilePath -> FilePath
buildAbsolutePathToWsFile fileName = lang </> "ws" </> fileName <.> "ws"

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
lang = "wsa"
