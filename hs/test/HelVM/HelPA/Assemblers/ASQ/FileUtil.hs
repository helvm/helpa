module HelVM.HelPA.Assemblers.ASQ.FileUtil (
  buildAbsolutePathToSqFile,
  buildAbsolutePathToIlFile,
  buildAbsolutePathToAsqFile,
  buildAsqFileName,
  asqDir,
  lang,
) where

import           System.FilePath.Posix

buildAbsolutePathToSqFile :: FilePath -> FilePath
buildAbsolutePathToSqFile fileName = lang </> "sq" </> fileName <.> "sq"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = lang </> "il" </> fileName <.> "il"

buildAbsolutePathToAsqFile :: FilePath -> FilePath
buildAbsolutePathToAsqFile fileName = asqDir </> buildAsqFileName fileName

buildAsqFileName :: FilePath -> FilePath
buildAsqFileName fileName = fileName <.> lang

asqDir :: FilePath
asqDir = dir </> "asq"

dir :: FilePath
dir = "examples"

lang :: FilePath
lang = "asq"
