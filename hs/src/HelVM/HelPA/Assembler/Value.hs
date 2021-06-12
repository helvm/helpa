module HelVM.HelPA.Assembler.Value where

data Value a = Literal a | Variable Identifier
  deriving stock (Eq, Show, Ord)

type NaturalValue = Value Natural

type IntegerValue = Value Integer

type StringValue = Value String

toIdentifier :: String -> Identifier
toIdentifier = toText

unwrapIdentifier :: Identifier -> String
unwrapIdentifier = toString

type Identifier = Text
