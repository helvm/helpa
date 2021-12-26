module HelVM.Common.Message.Message where

type MessageTuple = (Message , Message)

type Messages = DList.DList Text

type Message = Text

-- | Append Message

appendMessageTupleList :: MonadSafe m => [MessageTuple] -> m a -> m a
appendMessageTupleList = appendMessage . tupleListToMessage

appendMessageTuple :: MonadSafe m => MessageTuple -> m a -> m a
appendMessageTuple = appendMessage . tupleToMessage

appendMessage :: MonadSafe m => Message -> m a -> m a
appendMessage message a = catchMessage a appendAndThrow where appendAndThrow es = throwMessage (es `DList.snoc` message)

----

tupleListToMessage :: [MessageTuple] -> Message
tupleListToMessage xs = mconcat $ tupleToMessage <$> xs

tupleToMessage :: MessageTuple -> Message
tupleToMessage (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----