module HelVM.HelPA.Common.API where

absolutePath :: SourcePath -> SourcePath
absolutePath path = path { filePath = dirPath path <> "/" <> filePath path }

data SourcePath = SourcePath
  { dirPath :: String
  , filePath :: String
  }

----

type ParsedIO a = IO (Parsed a)

type Parsed a = Either String a

type ParsedExceptT a = ExceptT String IO a
