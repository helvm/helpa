module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import           HelVM.Common.Control.Control
import           HelVM.Common.Control.Logger
import           HelVM.Common.Control.Safe

type BIO m = (MonadControl m , BusinessIO m)

class Monad m => BusinessIO m where
  wReadFile :: FilePath -> m Text

instance BusinessIO IO where
  wReadFile = readFileText

instance BusinessIO (SafeT IO) where
  wReadFile = safeT . readFileText

instance BusinessIO (LoggerT IO) where
  wReadFile = loggerT . readFileText

instance BusinessIO (ControlT IO) where
  wReadFile = controlT . readFileText
