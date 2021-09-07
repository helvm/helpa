module HelVM.HelPA.Assemblers.ASQ.FileUtil (
  buildAbsolutePathToSqFile,
  buildAbsolutePathToIlFile,
  buildAbsolutePathToAsqFile,
  buildAsqFileName,
  asqDir
) where

import           System.FilePath.Posix

buildAbsolutePathToSqFile :: FilePath -> FilePath
buildAbsolutePathToSqFile fileName = asqDir </> "sq" </> fileName <.> "sq"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = asqDir </> "il" </> fileName <.> "il"

buildAbsolutePathToAsqFile :: FilePath -> FilePath
buildAbsolutePathToAsqFile fileName = asqDir </> buildAsqFileName fileName

buildAsqFileName :: FilePath -> FilePath
buildAsqFileName fileName = fileName <.> "asq"

asqDir :: FilePath
asqDir = dir </> "asq"

dir :: FilePath
dir = "examples"
