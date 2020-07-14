{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HelVM.HelPA.Assemblers.EAS.Parser where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.ParserUtil
import HelVM.HelPA.Common.Value

import Control.Monad.Combinators
import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

instance ShowErrorComponent Instruction where
  showErrorComponent = show

parseAssembler :: Text -> Either (ParseErrorBundle Text Void) InstructionList
parseAssembler = parse instructionListParser "Name of file"

instructionListParser :: Parser InstructionList
instructionListParser = skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace) <* eof

instructionParser :: Parser Instruction
instructionParser =
  try eParser
  <|> tParser
  <|> aParser
  <|> oParser
  <|> iParser
  <|> n0Parser
  <|> n1Parser
  <|> n2Parser
  <|> sParser
  <|> hParser
  <|> rParser
  <|> dParser
  <|> lParser
  <|> uParser
  <|> commentParser

----

eParser :: Parser Instruction
eParser = ((string' "E" <|> string' "dividE") *> endWordParser) Data.Functor.$>  E

tParser :: Parser Instruction
tParser = ((string' "T" <|> string' "Transfer") *> endWordParser) Data.Functor.$>  T

aParser :: Parser Instruction
aParser = ((string' "A" <|> string' "Address") *> endWordParser) Data.Functor.$>  A

oParser :: Parser Instruction
oParser = ((string' "O" <|> string' "Output") *> endWordParser) Data.Functor.$>  O

iParser :: Parser Instruction
iParser = ((string' "I" <|> string' "Input") *> endWordParser) Data.Functor.$>  I

n0Parser :: Parser Instruction
n0Parser = do N <$> naturalValueParser

n1Parser :: Parser Instruction
n1Parser =  do
  n <- string' "N" *> skipHorizontalSpace *> naturalValueParser
  return $ N n

n2Parser :: Parser Instruction
n2Parser = do
  n <- string' "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser
  return $ N n

sParser :: Parser Instruction
sParser = ((string' "S" <|> string' "Subtract") *> endWordParser) Data.Functor.$>  S

hParser :: Parser Instruction
hParser = ((string' "H" <|> string' "Halibut") *> endWordParser) Data.Functor.$>  H

rParser :: Parser Instruction
rParser = char '\n' Data.Functor.$>  R

dParser :: Parser Instruction
uParser = do
  identifier <- char '*' *> some letterChar
  return $ D identifier
  
lParser :: Parser Instruction
lParser = do L <$> (char '>' *> some letterChar <* char ':')

uParser :: Parser Instruction
dParser = do U <$> stringParser

commentParser :: Parser Instruction
commentParser = char commentChar *> skipAllToEndOfLine *> rParser

----

--skipAllToEndOfLine :: Parser 
skipAllToEndOfLine = takeWhileP Nothing isEndOfLine

--endWordParser :: Parser ()
--endWordParser = manyTill isEndWord
endWordParser = notFollowedBy alphaNumChar

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (c == commentChar)

commentChar :: Char
commentChar = '#'

----

naturalValueParser :: Parser NaturalValue
naturalValueParser = labelNaturalParser <|> naturalRightParser

labelNaturalParser :: Parser NaturalValue
labelNaturalParser = do
  identifier <- char '<' *> some letterChar
  return $ Variable identifier

naturalRightParser :: Parser NaturalValue
naturalRightParser = do Literal <$> naturalParser
