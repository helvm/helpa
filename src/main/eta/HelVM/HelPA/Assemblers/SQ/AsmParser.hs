{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.SQ.AsmParser where

import HelVM.HelPA.Assemblers.SQ.Instruction

import HelVM.HelPA.Common.AsmParserUtil
import HelVM.HelPA.Common.Value

import Control.Applicative
import Data.Attoparsec.Text hiding (I, D)
import Data.Char
import Data.Functor

import qualified Data.Text as T

parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser


--grammar
--program	:= list of intructions
--intruction := [.] list of items ( ';' | '\n' )
--item 	:= [label:]expression
--label	:= id
--expression := ( term | term+expression | term-expression )
--term 	:= ( -term | (expression) | const | id )
--const	:= ( number | 'letter' | ? )

--grammar
--program	:= list of intructions
--intruction := [.] list of items ( ';' | '\n' )
--item 	:= [label:]expression
--label	:= id
--expression := ( term | term+expression | term-expression )
--pm_expression := ( +expression | term-expression )

--term 	:= ( -term | (expression) | const | id )
--const	:= ( number | 'letter' | ? )

instructionListParser :: Parser InstructionList
instructionListParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser = do 
  d <- option (char '.')
  itemList <- many itemParser <* (char ';' <|> char '\n')
  return $ Instruction d itemList

itemParser :: Parser Item
itemParser = do 
  label <- option (labelParser <* char ':')
  expression <- expressionParser
  return $ Item label expression

--labelParser :: Parser Identifier
--labelParser = 

expressionParser :: Parser Expression
expressionParser = do 
  term <- termParser
  pmExpressionMaybe <- option pmExpressionParser
  return $ Expression term pmExpressionMaybe
  
pmExpressionParser :: Parser PMExpression
pmExpressionParser = do 
  pm <- char '+' <|> char '-'
  expression <- expressionParser
  return $ PMExpression pm expression
  
termParser :: Parser Term
termParser = 
      mTermParser
  <|> (char '(' *> expressionParser <* char ')'
  <|> constParser
  <|> naturalValueParser


mTermParser ::     
