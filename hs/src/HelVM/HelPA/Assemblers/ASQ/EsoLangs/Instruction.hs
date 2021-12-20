module HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction where

import           HelVM.HelPA.Assembler.Util
import           HelVM.HelPA.Assembler.Value

-- Original grammar
--program	:= list of intructions
--intruction := [.] list of items ( ';' | '\n' )
--item 	:= [label:]expression
--label	:= id
--expression := ( term | term+expression | term-expression )
--term 	:= ( -term | (expression) | const | id )
--const	:= ( number | 'letter' | ? )

-- Current grammar
--program	      := list of intructions
--instruction   := [.] list of items ( ';' | '\n' )
--item 	        := label: | expression | string
--label	        := id
--expression    :=  term [pm_expression]
--pm_expression := ( '+' expression | '-' expression )

--term 	        := value | '?' | -term | (expression)
--value	        := number | id

-- Functions
nonExpression :: Item -> Bool
nonExpression = not . isExpression

isExpression :: Item -> Bool
isExpression (ItemExpression _) = True
isExpression                 _  = False

execPM :: Num a => PM -> a -> a -> a
execPM Plus  = (+)
execPM Minus = (-)

-- Constructors for Instruction
makeDataInstructionFromIntegerValue :: IntegerValue -> Instruction
makeDataInstructionFromIntegerValue = makeDataInstruction . pure . makeItemFromIntegerValue

makeCodeInstructionFromIntegerValueList :: [IntegerValue] -> Instruction
makeCodeInstructionFromIntegerValueList l = makeCodeInstruction $ makeItemFromIntegerValue <$> l

makeDataInstruction :: ItemList -> Instruction
makeDataInstruction = Instruction Data

makeCodeInstruction :: ItemList -> Instruction
makeCodeInstruction = Instruction Code

-- Constructors for Item
makeItemFromIntegerValue :: IntegerValue -> Item
makeItemFromIntegerValue = ItemExpression . makeExpressionFromIntegerValue

-- Constructors for Expression
makeCurrentAddress :: Expression
makeCurrentAddress = makeExpressionWithoutPM TermQuestionMark

makeNextAddress :: Expression
makeNextAddress = makeExpressionWithPM (PMExpression Plus makeOne) TermQuestionMark

makePrevAddress :: Expression
makePrevAddress = makeExpressionWithPM (PMExpression Minus makeOne) TermQuestionMark

makeOne :: Expression
makeOne = makeExpressionFromInteger 1

makeExpressionFromChar :: Char -> Expression
makeExpressionFromChar = makeExpressionFromInteger . toInteger . ord

makeExpressionFromIdentifier :: Identifier -> Expression
makeExpressionFromIdentifier = makeExpressionFromIntegerValue . Variable

makeExpressionFromInteger :: Integer -> Expression
makeExpressionFromInteger = makeExpressionFromIntegerValue . Literal

makeExpressionFromIntegerValue :: IntegerValue -> Expression
makeExpressionFromIntegerValue = makeExpressionWithoutPM . TermSymbol

makeExpressionWithoutPM :: Term -> Expression
makeExpressionWithoutPM = makeExpression Nothing

makeExpressionWithPM :: PMExpression -> Term -> Expression
makeExpressionWithPM pm = makeExpression (Just pm)

makeExpression :: Maybe PMExpression -> Term -> Expression
makeExpression = Expression

-- Types
type InstructionList = [Instruction]

data Instruction = Instruction InstructionType !ItemList
  deriving stock (Eq, Show, Ord)

data InstructionType = Data | Code
  deriving stock (Eq, Show, Ord)

type ItemList = [Item]

data Item = ItemLabel Label | ItemExpression Expression | ItemString String
  deriving stock (Eq, Show, Ord)

type LabelList = [Label]

type Label = Identifier

type ExpressionWithSymbol = WithSymbol Expression

type ExpressionList = [Expression]

data Expression = Expression !(Maybe PMExpression) !Term
  deriving stock (Eq, Show, Ord)

data PMExpression = PMExpression !PM !Expression
  deriving stock (Eq, Show, Ord)

data PM = Plus | Minus
  deriving stock (Eq, Show, Ord)

data Term = TermSymbol IntegerValue | TermQuestionMark | TermMinus !Term | TermExpression !Expression
  deriving stock (Eq, Show, Ord)
