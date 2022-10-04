module HelVM.HelPA.Assemblers.WSA.API.TokenType where

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
