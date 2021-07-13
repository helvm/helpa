{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import HelVM.Common.Safe

type BIO m = (MonadSafeError m , BusinessIO m)

class (Monad m , MonadIO m) => BusinessIO m where
  wReadFile :: FilePath -> m Text
--  wReadFile = liftIO . readFileText

instance BusinessIO IO where
  wReadFile = readFileText

instance (Monad m , MonadIO m) => BusinessIO m where
  wReadFile = liftIO . readFileText
