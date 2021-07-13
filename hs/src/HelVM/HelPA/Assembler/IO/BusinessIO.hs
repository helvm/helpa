--{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelPA.Assembler.IO.BusinessIO (
  BIO,
  BusinessIO,
  wReadFile,
) where

import HelVM.Common.Safe
import HelVM.Common.SafeMonadT

type BIO m = (MonadSafeError m , BusinessIO m)

class Monad m => BusinessIO m where
  wReadFile :: FilePath -> m Text
--  wReadFile = liftIO . readFileText

instance BusinessIO IO where
  wReadFile = readFileText

instance BusinessIO (SafeMonadT IO) where
  wReadFile = hoistMonad . readFileText

--instance (Monad m , MonadIO m) => BusinessIO m where
--  wReadFile = liftIO . readFileText
