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
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace) -- <* endOfInput

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
    where zeroOperandInstruction i ts = (asciiCIChoices ts *> endWordParser) $> i

numberOperandInstructionParser :: Parser Instruction
numberOperandInstructionParser = N <$> (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

rParser :: Parser Instruction
rParser = (skipMany1EndLine *> skipManyComment) $> R

dParser :: Parser Instruction
dParser = D <$> (char '*' *> fileNameParser <* char '\n')
  
lParser :: Parser Instruction
lParser = L <$> (char '>' *> identifierParser <* char ':')

uParser :: Parser Instruction
uParser = U <$> stringParser

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
isEndWord c = isSpace c || (c == commentChar)

commentChar :: Char
commentChar = '#'

----

naturalValueParser :: Parser NaturalValue
naturalValueParser = labelNaturalParser <|> naturalRightParser

labelNaturalParser :: Parser NaturalValue
labelNaturalParser = Variable <$> (char '<' *> many1 letter)

naturalRightParser :: Parser NaturalValue
naturalRightParser = Literal <$> naturalParser

----

