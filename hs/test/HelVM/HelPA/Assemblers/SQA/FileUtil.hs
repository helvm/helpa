module HelVM.HelPA.Assemblers.SQA.FileUtil (
  buildAbsolutePathToSqFile,
  buildAbsolutePathToIlFile,
  buildAbsolutePathToSqaFile,
  buildSqaFileName,
  sqaDir
) where

import           System.FilePath.Posix

buildAbsolutePathToSqFile :: FilePath -> FilePath
buildAbsolutePathToSqFile fileName = sqaDir </> "sq" </> fileName <.> "sq"

buildAbsolutePathToIlFile :: FilePath -> FilePath
buildAbsolutePathToIlFile fileName = sqaDir </> "il" </> fileName <.> "il"

buildAbsolutePathToSqaFile :: FilePath -> FilePath
buildAbsolutePathToSqaFile fileName = sqaDir </> buildSqaFileName fileName

buildSqaFileName :: FilePath -> FilePath
buildSqaFileName fileName = fileName <.> "sqa"

sqaDir :: FilePath
sqaDir = dir </> "sqa"

dir :: FilePath
dir = "examples"
