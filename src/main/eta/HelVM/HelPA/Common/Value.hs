module HelVM.HelPA.Common.Value where

import Numeric.Natural

data Value a = Literal a | Variable Identifier
  deriving (Eq, Show, Ord)

type NaturalValue = Value Natural 

type IntegerValue = Value Integer 

type StringValue = Value String 

type Identifier = String

data PlusMinus = Plus | Minus
  deriving (Eq, Show, Ord)

toPlusMinus :: Char -> PlusMinus
toPlusMinus '+' = Plus
toPlusMinus '-' = Minus
toPlusMinus c   = error $ "? " <> charToString c
