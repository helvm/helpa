module HelVM.HelPA.Assemblers.WSA.FileUtil  where

import System.FilePath.Posix

buildAbsolutePathToWsFile :: String -> String
buildAbsolutePathToWsFile fileName = wsaDir </> "ws" </> fileName <.> "ws"

buildAbsolutePathToIlFile :: String -> String
buildAbsolutePathToIlFile fileName = wsaDir </> "il" </> fileName <.> "il"

libDir :: String
libDir = wsaDir </> "libs"

appDir :: String
appDir = wsaDir </> "apps"

wsDir :: String
wsDir = dir </> "ws"

wsaDir :: String
wsaDir = dir </> "wsa"

dir :: String
dir = "examples"

ext :: String
ext = "wsa"
