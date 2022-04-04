module HelVM.HelPA.Assemblers.WSA.Token where

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import           HelVM.Common.Containers.Util
import           HelVM.Common.Control.Safe
import           HelVM.Common.Digit.Digitable
import           HelVM.Common.Digit.ToDigit

import           Text.Read

import qualified Text.Show

showTLByType :: TokenType -> TokenList -> Text
showTLByType VisibleTokenType = showTL
showTLByType WhiteTokenType   = showTLAsWTL
showTLByType BothTokenType    = showTLAsBTL

showTL :: TokenList -> Text
showTL = showFoldable

showTLAsWTL :: TokenList -> Text
showTLAsWTL = showFoldable . toWTL

toWTL :: TokenList -> WhiteTokenList
toWTL = map WhiteToken

showTLAsBTL :: TokenList -> Text
showTLAsBTL = showFoldable . toBTL

toBTL :: TokenList -> BothTokenList
toBTL = map BothToken

----

type TokenList = [Token]

data Token =  S | T | N | E | R
  deriving stock (Bounded , Enum , Eq)

instance Show Token where
  show S = "S"
  show T = "T"
  show N = "N"
  show E = " "
  show R = "\n"

instance ToDigit Token where
  toDigit S = pure 0
  toDigit T = pure 1
  toDigit t = liftError $ show t

instance Digitable Token where
  fromDigit 0 = pure S
  fromDigit 1 = pure T
  fromDigit t = liftError $ show t

----

type WhiteTokenList = [WhiteToken]

newtype WhiteToken = WhiteToken { unWhiteToken :: Token }
  deriving stock (Bounded , Eq)

instance Show WhiteToken where
  show (WhiteToken S) = " "
  show (WhiteToken T) = "\t"
  show (WhiteToken N) = "\n"
  show (WhiteToken _) = ""

-- | Scanner
instance Read WhiteToken where
  readsPrec _ " "  = [( WhiteToken S , "")]
  readsPrec _ "\t" = [( WhiteToken T , "")]
  readsPrec _ "\n" = [( WhiteToken N , "")]
  readsPrec _ _    = []

----

type BothTokenList = [BothToken]

newtype BothToken = BothToken { unBothToken :: Token }
  deriving stock (Bounded , Eq)

instance Show BothToken where
  show (BothToken S) = " S"
  show (BothToken T) = "\tT"
  show (BothToken N) = "\nN"
  show (BothToken _) = ""
