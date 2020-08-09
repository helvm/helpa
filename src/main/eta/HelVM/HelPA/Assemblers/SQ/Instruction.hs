module HelVM.HelPA.Assemblers.SQ.Instruction where

import HelVM.HelPA.Common.Value

data Const = LiteralInteger Integer | LiteralString String | QuestionMark
  deriving (Eq, Show, Ord)

data Term = Minus Term
  deriving (Eq, Show, Ord)

data PMExpression = PMExpression Bool Expression
  deriving (Eq, Show, Ord)

data Expression = Expression Term (Maybe PMExpression)
  deriving (Eq, Show, Ord)

data Item = Item (Maybe Identifier) Expression
  deriving (Eq, Show, Ord)
  
type ItemList = [Item]  

data Instruction = Instruction Bool ItemList
  deriving (Eq, Show, Ord)

type InstructionList = [Instruction]

