module HelVM.HelPA.Common.Value where

import Numeric.Natural

data Value a = Literal a | Variable Identifier
  deriving (Eq, Show, Ord)

type NaturalValue = Value Natural 

type IntegerValue = Value Integer 

type StringValue = Value String 

type Identifier = String
