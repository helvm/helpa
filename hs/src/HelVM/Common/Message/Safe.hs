module HelVM.Common.Message.Safe (
  safeIOToPTextIO,

  safeIOToIO,
  safeToIO,
  exceptTToIO,
  safeExceptT,
  safeToEitherLegacy,
  MessagesToText,

  liftExceptT,
  liftSafe,
  liftEitherMessage,
  liftEitherLegacy,

  liftMaybeOrMessageTupleList,
  liftMaybeOrMessageTuple,
  liftMaybeOrMessage,

  liftMessageTupleList,
  liftMessageTuple,
  liftMessage,

  appendMessageTupleList,
  appendMessageTuple,
  appendMessage,

  tupleListToMessage,
  tupleToMessage,

  MonadSafe,
  SafeExceptT,
  EitherLegacy,
  EitherMessage,
  Safe,
  Message,
) where

import           HelVM.Common.Message.Message

import           HelVM.Common.Util

import           Control.Monad.Except         hiding (ExceptT, runExceptT)

import           System.IO.Message

import qualified Data.DList                   as DList

safeIOToPTextIO :: Show a => IO (Safe a) -> IO Text
safeIOToPTextIO a = showP <$> safeIOToIO a

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = exceptTToIO . liftSafe

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT (userMessage . MessagesToString)

safeExceptT :: Monad m => m a -> SafeExceptT m a
safeExceptT a = ExceptT $ pure <$> a

safeToEitherLegacy :: Safe a -> EitherLegacy a
safeToEitherLegacy = first MessagesToString

MessagesToString :: Messages -> String
MessagesToString = toString . MessagesToText

MessagesToText :: Messages -> Text
MessagesToText = unlines . DList.toList

-- | Lift

liftExceptT :: MonadMessage e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafe m => Safe a -> m a
liftSafe = liftEither

liftEitherMessage :: MonadSafe m => Either Text a -> m a
liftEitherMessage = liftSafe . first DList.singleton

liftEitherLegacy :: MonadSafe m => EitherLegacy a -> m a
liftEitherLegacy = liftSafe . first stringToMessages

stringToMessages :: String -> Messages
stringToMessages = DList.singleton . toText

-- | Lift from Maybe

liftMaybeOrMessageTupleList :: MonadSafe m => [MessageTuple] -> Maybe a -> m a
liftMaybeOrMessageTupleList = liftMaybeOrMessage . tupleListToMessage

liftMaybeOrMessageTuple :: MonadSafe m => MessageTuple -> Maybe a -> m a
liftMaybeOrMessageTuple = liftMaybeOrMessage . tupleToMessage

liftMaybeOrMessage :: MonadSafe m => Message -> Maybe a -> m a
liftMaybeOrMessage e = liftSafe . maybeToRight (DList.singleton e)

-- | Lift from Message

liftMessageTupleList :: MonadSafe m => [MessageTuple] -> m a
liftMessageTupleList = liftMessage . tupleListToMessage

liftMessageTuple :: MonadSafe m => MessageTuple -> m a
liftMessageTuple = liftMessage . tupleToMessage

liftMessage :: MonadSafe m => Message -> m a
liftMessage = throwMessage . DList.singleton

type MonadSafe m = (MonadSafeMessage m)

type MonadSafeMessage m = (MonadMessage Messages m)

type SafeExceptT m = ExceptT Messages m

type EitherLegacy = Either String

type EitherMessage = Either Text

type Safe = Either Messages
