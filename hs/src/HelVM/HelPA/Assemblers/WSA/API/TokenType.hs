module HelVM.HelPA.Assemblers.WSA.API.TokenType where

parseTokenType :: String -> TokenType
parseTokenType raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
