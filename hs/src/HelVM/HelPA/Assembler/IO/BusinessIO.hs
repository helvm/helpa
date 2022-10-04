module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Logger
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

type BIO m = (MonadControl m , BusinessIO m)

class Monad m => BusinessIO m where
  wReadFile :: FilePath -> m Text

instance BusinessIO IO where
  wReadFile = readFileTextUtf8

instance BusinessIO (SafeT IO) where
  wReadFile = safeT . readFileTextUtf8

instance BusinessIO (LoggerT IO) where
  wReadFile = loggerT . readFileTextUtf8

instance BusinessIO (ControlT IO) where
  wReadFile = controlT . readFileTextUtf8
