module HelVM.HelPA.Assemblers.EAS.FileUtil (
  readEtaFile,
  buildAbsolutePathToEtaFile,
  buildAbsolutePathToEasFile,
  buildEasFileName,
  easDir
) where

import System.FilePath.Posix

readEtaFile :: String -> IO String
readEtaFile fileName = readFile $ buildAbsolutePathToEtaFile fileName

buildAbsolutePathToEtaFile :: String -> String
buildAbsolutePathToEtaFile fileName = dir </> "eta" </> fileName <.> "eta"

buildAbsolutePathToEasFile :: String -> String
buildAbsolutePathToEasFile fileName = easDir </> buildEasFileName fileName

buildEasFileName :: String -> String
buildEasFileName fileName = fileName <.> "eas"

easDir :: String
easDir = dir </> "eas"

dir :: String
dir = "examples"
