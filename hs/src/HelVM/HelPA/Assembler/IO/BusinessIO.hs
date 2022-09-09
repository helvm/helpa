module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Logger
import           HelVM.HelIO.Control.Safe

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
