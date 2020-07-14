{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.EAS.AsmParser where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.AsmParserUtil
import HelVM.HelPA.Common.Value

import Control.Applicative
import Data.Attoparsec.Text hiding (I, D)
import Data.Char
import Data.Functor

import qualified Data.Text as T

parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace)

instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> numberOperandInstructionParser
  <|> rParser
  <|> dParser
  <|> lParser
  <|> uParser
  <|> commentParser

----

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser =
      zeroOperandInstruction E ["E", "dividE"]
  <|> zeroOperandInstruction T ["T", "Transfer"]
  <|> zeroOperandInstruction A ["A", "Address"]
  <|> zeroOperandInstruction O ["O", "Output"]
  <|> zeroOperandInstruction I ["I", "Input"]
  <|> zeroOperandInstruction S ["S", "Subtract"]
  <|> zeroOperandInstruction H ["H", "Halibut"]
    where zeroOperandInstruction i ts = asciiCIChoices ts *> endWordParser *> pure i

numberOperandInstructionParser :: Parser Instruction
numberOperandInstructionParser = fmap N (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

rParser :: Parser Instruction
rParser = skipMany1EndLine *> skipManyComment *> pure R

dParser :: Parser Instruction
dParser = fmap D $ char '*' *> identifierParser
  
lParser :: Parser Instruction
lParser = fmap L $ char '>' *> identifierParser <* char ':'

uParser :: Parser Instruction
uParser = fmap U stringParser

commentParser :: Parser Instruction
commentParser = skipComment *> rParser

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')

----

endWordParser :: Parser T.Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = (isSpace c) || (c == commentChar)

commentChar :: Char
commentChar = '#'

----

naturalValueParser :: Parser NaturalValue
naturalValueParser = labelNaturalParser <|> naturalRightParser

labelNaturalParser :: Parser NaturalValue
labelNaturalParser = fmap Variable $ char '<' *> many1 letter

naturalRightParser :: Parser NaturalValue
naturalRightParser = fmap Literal $ naturalParser

----

