module HelVM.HelPA.Assembler.Value where

import           HelVM.HelIO.Collections.SList

data Value a
  = Literal !a
  | Variable !Identifier
  deriving stock (Eq , Read , Show)

type NaturalValue = Value Natural

type IntegerValue = Value Integer

type StringValue = Value SString

toIdentifier :: String -> Identifier
toIdentifier = toText

unwrapIdentifier :: Identifier -> String
unwrapIdentifier = toString

type Identifier = Text
