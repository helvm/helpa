module HelVM.HelPA.Assemblers.WSA.Token where

import HelVM.HelPA.Common.TokenType

import Data.Char
import Text.Read

import qualified Text.Show

showTLByType :: TokenType -> TokenList -> String
showTLByType VisibleTokenType = showTL
showTLByType WhiteTokenType   = showTLAsWTL
showTLByType BothTokenType    = showTLAsBTL

showTL :: TokenList -> String
showTL tl = show =<< tl

showTLAsWTL :: TokenList -> String
showTLAsWTL = showWTL . toWTL

toWTL :: TokenList -> WhiteTokenList
toWTL = map WhiteToken

showWTL :: WhiteTokenList -> String
showWTL tl = show =<< tl

showTLAsBTL :: TokenList -> String
showTLAsBTL = showBTL . toBTL

toBTL :: TokenList -> BothTokenList
toBTL = map BothToken

showBTL :: BothTokenList -> String
showBTL tl = show =<< tl

----

toDigit :: (Num a) => Token -> a
toDigit S = 0
toDigit T = 1
toDigit c = error $ show c

toBitChar :: Token -> Char
toBitChar = intToDigit . toDigit

bitToToken :: Natural -> Token
bitToToken 0 = S
bitToToken 1 = T
bitToToken a = error $ show a

type TokenList = [Token]

data Token =  S | T | N | E | R
  deriving (Eq, Ord, Enum, Read)

instance Show Token where
  show S = "S"
  show T = "T"
  show N = "N"
  show E = " "
  show R = "\n"

----

type WhiteTokenList = [WhiteToken]

newtype WhiteToken = WhiteToken Token deriving (Eq)

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

newtype BothToken = BothToken Token deriving (Eq)

instance Show BothToken where
  show (BothToken S) = " S"
  show (BothToken T) = "\tT"
  show (BothToken N) = "\nN"
  show (BothToken _) = ""
