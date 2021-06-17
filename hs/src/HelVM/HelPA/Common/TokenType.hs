module HelVM.HelPA.Common.TokenType where

import RIO
  
data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType deriving (Eq , Read , Show)

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

parseTokenType :: String -> TokenType
parseTokenType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "TokenType '" <> raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes
