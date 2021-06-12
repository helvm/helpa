module HelVM.HelPA.Assemblers.WSA.FileUtil where

import System.FilePath.Posix

buildAbsolutePathToWsFile :: FilePath -> FilePath
buildAbsolutePathToWsFile fileName = wsaDir </> "ws" </> fileName <.> "ws"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = wsaDir </> "il" </> fileName <.> "il"

libDir :: FilePath
libDir = wsaDir </> "libs"

appDir :: FilePath
appDir = wsaDir </> "apps"

wsDir :: FilePath
wsDir = dir </> "ws"

wsaDir :: FilePath
wsaDir = dir </> "wsa"

dir :: FilePath
dir = "examples"

ext :: FilePath
ext = "wsa"
