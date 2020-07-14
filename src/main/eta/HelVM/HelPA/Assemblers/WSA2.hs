module HelVM.HelPA.Assemblers.WSA where

data Token
  = IntegerValue Integer
  , StringValue String
  , CharValue Char
  , Copy
  , Dup
  , Swap
  , Add
  , Sub
  , Mul
  , Div
  , Mod 
  , Load
  , Store
  , Mark 
  , Call
  , Jump
  , JumpZero
  , JumpNeg
  , Return
  , End
  , Output
  , OutputNumber
  , Input
  , InputNumber
  , Identifier
 


-- condition label/address jump

-- condition jump label/address

-- condition 

