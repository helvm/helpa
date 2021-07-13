module HelVM.Common.Safe (
  safeIOToIO,
  safeToIO,
  exceptTToIO,
  userErrorText,

  hoistMonad,
  liftExceptT,
  liftSafe,
  liftLegacySafe,
  
  liftMaybeOrErrorTupleList,
  liftMaybeOrErrorTuple,
  liftMaybeOrError,

  liftErrorTupleList,
  liftErrorTuple,
  liftError,

  safe,
  legacySafeToSafe,
  safeToLegacySafe,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  tupleListToError,
  tupleToError,

  unsafe,

  MonadSafeError,
  SafeExceptT,
  Safe,
  Error,
) where

import Control.Exception.Base
import Control.Monad.Except hiding (ExceptT , runExceptT)

import System.IO.Error

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = exceptTToIO . liftSafe

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT userErrorText

userErrorText :: Text -> IOException
userErrorText = userError . toString

---- Lift

hoistMonad :: Monad m => m a -> SafeExceptT m a
hoistMonad a = ExceptT $ safe <$> a

liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafeError m => Safe a -> m a
liftSafe = liftEither

liftLegacySafe :: MonadSafeError m => LegacySafe a -> m a
liftLegacySafe = liftSafe . legacySafeToSafe

-- Lift from Maybe

liftMaybeOrErrorTupleList :: MonadSafeError m => [ErrorTuple] -> Maybe a -> m a
liftMaybeOrErrorTupleList = liftMaybeOrError . tupleListToError

liftMaybeOrErrorTuple :: MonadSafeError m => ErrorTuple -> Maybe a -> m a
liftMaybeOrErrorTuple = liftMaybeOrError . tupleToError

liftMaybeOrError :: MonadSafeError m => Error -> Maybe a -> m a
liftMaybeOrError e = liftSafe . maybeToRight e

-- Lift from Error

liftErrorTupleList :: MonadSafeError m => [ErrorTuple] -> m a
liftErrorTupleList = liftError . tupleListToError

liftErrorTuple :: MonadSafeError m => ErrorTuple -> m a
liftErrorTuple = liftError . tupleToError

liftError :: MonadSafeError m => Error -> m a
liftError = throwError

-- Create Safe

safe :: a -> Safe a
safe = pure

legacySafeToSafe :: LegacySafe a -> Safe a
legacySafeToSafe = first toText

safeToLegacySafe :: Safe a -> LegacySafe a
safeToLegacySafe = first toString

---- Append Error

appendErrorTupleList :: [ErrorTuple] -> Safe a -> Safe a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: ErrorTuple -> Safe a -> Safe a
appendErrorTuple = appendError . tupleToError

appendError :: Error -> Safe a -> Safe a
appendError message = first (<> message)

----

tupleListToError :: [ErrorTuple] -> Error
tupleListToError xs = mconcat $ tupleToError <$> xs

tupleToError :: ErrorTuple -> Error
tupleToError (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----

unsafe :: Safe a -> a
unsafe (Right a) = a
unsafe (Left a) = error a

----

type MonadSafeError m = MonadError Error m

type SafeExceptT m = ExceptT Error m

type LegacySafe = Either String

type Safe = Either Error

type ErrorTuple = (Error , Error)

type Error = Text
