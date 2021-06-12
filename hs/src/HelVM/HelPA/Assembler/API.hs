module HelVM.HelPA.Assembler.API where

absolutePath :: SourcePath -> SourcePath
absolutePath path = path { filePath = dirPath path <> "/" <> filePath path }

data SourcePath = SourcePath
  { dirPath :: FilePath
  , filePath :: FilePath
  }
