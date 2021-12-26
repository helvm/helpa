module HelVM.Common.Safe (
  safeIOToPTextIO,

  safeIOToIO,
  safeToIO,
  exceptTToIO,
  safeExceptT,
  safeToEitherLegacy,
  errorsToText,

  liftExceptT,
  liftSafe,
  liftEitherError,
  liftEitherLegacy,

  liftMaybeOrErrorTupleList,
  liftMaybeOrErrorTuple,
  liftMaybeOrError,

  liftErrorTupleList,
  liftErrorTuple,
  liftError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  tupleListToError,
  tupleToError,

  MonadSafe,
  SafeExceptT,
  EitherLegacy,
  EitherError,
  Safe,
  Error,
) where

import           HelVM.Common.Util

import           Control.Monad.Except      hiding (ExceptT, runExceptT)

import           Control.Monad.Writer.Lazy

--import           Colog.Monad

import           Control.Monad.Logger

import           System.IO.Error

import qualified Data.DList                as DList

safeIOToPTextIO :: Show a => IO (Safe a) -> IO Text
safeIOToPTextIO a = showP <$> safeIOToIO a

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = exceptTToIO . liftSafe

--FIXME
--writerTToIO :: SafeWriterT IO a -> IO a
--writerTToIO = liftWriterT . evalWriter

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT (userError . errorsToString)

--aaa :: MonadSafe m => m a -> SafeExceptT IO a
--aaa :: (MonadSafe m , MonadSafeError m2) => m a -> m2 a
--aaa = id

evalWriter :: Writer c a -> a
evalWriter = fst . runWriter

safeExceptT :: Monad m => m a -> SafeExceptT m a
safeExceptT a = ExceptT $ pure <$> a



safeToEitherLegacy :: Safe a -> EitherLegacy a
safeToEitherLegacy = first errorsToString

errorsToString :: Errors -> String
errorsToString = toString . errorsToText

errorsToText :: Errors -> Text
errorsToText = unlines . DList.toList

-- | Lift

--liftWriterT :: MonadError e m => WriterT e m a -> m a
--liftWriterT m = writer =<< runWriterT m

liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafe m => Safe a -> m a
liftSafe = liftEither

liftEitherError :: MonadSafe m => Either Text a -> m a
liftEitherError = liftSafe . first DList.singleton

liftEitherLegacy :: MonadSafe m => EitherLegacy a -> m a
liftEitherLegacy = liftSafe . first stringToErrors

stringToErrors :: String -> Errors
stringToErrors = DList.singleton . toText

-- | Lift from Maybe

liftMaybeOrErrorTupleList :: MonadSafe m => [ErrorTuple] -> Maybe a -> m a
liftMaybeOrErrorTupleList = liftMaybeOrError . tupleListToError

liftMaybeOrErrorTuple :: MonadSafe m => ErrorTuple -> Maybe a -> m a
liftMaybeOrErrorTuple = liftMaybeOrError . tupleToError

liftMaybeOrError :: MonadSafe m => Error -> Maybe a -> m a
liftMaybeOrError e = liftSafe . maybeToRight (DList.singleton e)

-- | Lift from Error

liftErrorTupleList :: MonadSafe m => [ErrorTuple] -> m a
liftErrorTupleList = liftError . tupleListToError

liftErrorTuple :: MonadSafe m => ErrorTuple -> m a
liftErrorTuple = liftError . tupleToError

liftError :: MonadSafe m => Error -> m a
liftError = throwError . DList.singleton

-- | Append Error

appendErrorTupleList :: MonadSafe m => [ErrorTuple] -> m a -> m a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: MonadSafe m => ErrorTuple -> m a -> m a
appendErrorTuple = appendError . tupleToError

appendError :: MonadSafe m => Error -> m a -> m a
appendError message a = catchError a appendAndThrow where appendAndThrow es = throwError (es `DList.snoc` message)

----

tupleListToError :: [ErrorTuple] -> Error
tupleListToError xs = mconcat $ tupleToError <$> xs

tupleToError :: ErrorTuple -> Error
tupleToError (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----

--type MonadSafe m = (MonadSafeError m , WithLog env Message m)
type MonadSafe m = (MonadSafeError m , MonadLogger m)
--type MonadSafe m = (MonadSafeError m)

type MonadSafeError m = (MonadError Errors m)

--type SafeWriterT m = WriterT Errors m

type SafeExceptT m = ExceptT Errors m

type EitherLegacy = Either String

type EitherError = Either Text

type Safe = Either Errors

type ErrorTuple = (Error , Error)

type Errors = DList.DList Text

type Error = Text
