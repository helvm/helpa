{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  wReadFile,
  BusinessIO,
) where

import HelVM.Common.Safe

--type BIO m = (WithLog env Text m , MonadSafeError m , BusinessIO m)
type BIO m = (MonadSafeError m , BusinessIO m)

class (Monad m , MonadIO m) => BusinessIO m where
  wReadFile :: FilePath -> m Text
--  wReadFile = liftIO . readFileText

instance BusinessIO IO where
  wReadFile = readFileText

instance (Monad m , MonadIO m) => BusinessIO m where
  wReadFile = liftIO . readFileText
