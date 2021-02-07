module HelVM.HelPA.Assemblers.EAS.AsmParser (
  parseAssemblyText
) where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.API
import HelVM.HelPA.Common.AsmParserUtil
import HelVM.HelPA.Common.Value

import Data.Attoparsec.Text hiding (I, D)
import Data.Char

parseAssemblyText :: Text -> Parsed InstructionList
parseAssemblyText = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace) -- <* endOfInput

instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> naturalNumberParser
  <|> unescapedStringParser
  <|> labelDefinitionParser
  <|> includeFileParser
  <|> lineBreakParser
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
    where zeroOperandInstruction i ts = i <$ (asciiCIChoices ts *> endWordParser)

naturalNumberParser :: Parser Instruction
naturalNumberParser = N <$> (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

unescapedStringParser :: Parser Instruction
unescapedStringParser = U <$> stringParser

labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')

lineBreakParser :: Parser Instruction
lineBreakParser = R <$ (skipMany1EndLine *> skipManyComment)

commentParser :: Parser Instruction
commentParser = skipComment *> lineBreakParser

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')

----

endWordParser :: Parser Text
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
