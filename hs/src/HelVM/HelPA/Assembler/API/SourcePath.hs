module HelVM.HelPA.Assembler.API.SourcePath where

import           System.FilePath.Posix

absolutePath :: SourcePath -> SourcePath
absolutePath path = path { filePath = dirPath path </> filePath path }

data SourcePath = SourcePath
  { dirPath  :: !FilePath
  , filePath :: !FilePath
  }
