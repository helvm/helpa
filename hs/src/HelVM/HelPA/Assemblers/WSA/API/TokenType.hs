module HelVM.HelPA.Assemblers.WSA.API.TokenType where

import           HelVM.HelIO.Extra

parseTokenType :: String -> TokenType
parseTokenType raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
