module HelVM.HelPA.Assembler.IO.WrapperIO (
  wReadFile,
  WrapperIO,
) where

class Monad m => WrapperIO m where
  wReadFile :: FilePath -> m Text

instance WrapperIO IO where
  wReadFile = readFileText
