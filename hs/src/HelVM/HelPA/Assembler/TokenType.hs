module HelVM.HelPA.Assembler.TokenType where

data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType
  deriving stock (Eq , Read , Show)

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

parseTokenType :: String -> TokenType
parseTokenType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "TokenType '" <> toText raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes
