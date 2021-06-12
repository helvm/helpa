module HelVM.HelPA.Assemblers.WSA.Token where

import HelVM.HelPA.Assembler.TokenType

import HelVM.Common.Digit.Digitable
import HelVM.Common.Digit.ToDigit
import HelVM.Common.Safe
import HelVM.Common.Containers.Util

import Text.Read

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
  deriving stock (Eq, Ord, Enum, Read)

instance Show Token where
  show S = "S"
  show T = "T"
  show N = "N"
  show E = " "
  show R = "\n"

instance ToDigit Token where
  toDigit S = safe 0
  toDigit T = safe 1
  toDigit t = safeError $ show t

instance Digitable Token where
  fromDigit 0 = safe S
  fromDigit 1 = safe T
  fromDigit t = safeError $ show t

----

type WhiteTokenList = [WhiteToken]

newtype WhiteToken = WhiteToken Token
  deriving stock (Eq)

instance Show WhiteToken where
  show (WhiteToken S) = " "
  show (WhiteToken T) = "\t"
  show (WhiteToken N) = "\n"
  show (WhiteToken _) = ""

-- Scanner
instance Read WhiteToken where
  readsPrec _ " "  = [( WhiteToken S , "")]
  readsPrec _ "\t" = [( WhiteToken T , "")]
  readsPrec _ "\n" = [( WhiteToken N , "")]
  readsPrec _ _    = []

----

type BothTokenList = [BothToken]

newtype BothToken = BothToken Token
  deriving stock (Eq)

instance Show BothToken where
  show (BothToken S) = " S"
  show (BothToken T) = "\tT"
  show (BothToken N) = "\nN"
  show (BothToken _) = ""
