module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import HelVM.Common.Safe

type BIO m = (MonadSafeError m , BusinessIO m)

class Monad m => BusinessIO m where
  wReadFile :: FilePath -> m Text

instance BusinessIO IO where
  wReadFile = readFileText

instance BusinessIO (SafeExceptT IO) where
  wReadFile = safeExceptT . readFileText
