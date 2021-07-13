module HelVM.Common.ReadText (
  readText,
  readTextSafe,
  readTextMaybe,
) where

import HelVM.Common.Safe

readText :: Read a => Text -> a
readText = unsafe . readTextSafe

readTextSafe :: (MonadSafeError m , Read a) => Text -> m a
readTextSafe a = appendErrorTuple ("" , a) $ liftSafe $ readEither $ toString a

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . toString
