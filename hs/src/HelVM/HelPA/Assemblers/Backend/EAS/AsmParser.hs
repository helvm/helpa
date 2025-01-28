module HelVM.HelPA.Assemblers.Backend.EAS.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Backend.EAS.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe

import           Data.Attoparsec.Text                           hiding (D, I)
import           Data.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <* endOfInput)

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace)

instructionParser :: Parser Instruction
instructionParser = choice
  [ zeroOperandInstructionParser
  , naturalNumberParser
  , unescapedStringParser
  , labelDefinitionParser
  , includeFileParser
  , lineBreakParser
  , commentParser
  ]

----

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser = choice
  [ zeroOperandInstruction E ["E", "dividE"]
  , zeroOperandInstruction T ["T", "Transfer"]
  , zeroOperandInstruction A ["A", "Address"]
  , zeroOperandInstruction O ["O", "Output"]
  , zeroOperandInstruction I ["I", "Input"]
  , zeroOperandInstruction S ["S", "Subtract"]
  , zeroOperandInstruction H ["H", "Halibut"]
  ] where zeroOperandInstruction i ts = i <$ (asciiCIChoices ts *> endWordParser)

naturalNumberParser :: Parser Instruction
naturalNumberParser = N <$> choice
  [ labelNaturalValueParser
  , asciiCI "N" *> skipHorizontalSpace *> labelNaturalValueParser
  , asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> labelNaturalValueParser
  ]

unescapedStringParser :: Parser Instruction
unescapedStringParser = U . fromString <$> stringParser

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

labelNaturalValueParser :: Parser NaturalValue
labelNaturalValueParser = labelNaturalParser <|> naturalRightParser

labelNaturalParser :: Parser NaturalValue
labelNaturalParser = Variable . toIdentifier <$> (char '<' *> many1 letter)

naturalRightParser :: Parser NaturalValue
naturalRightParser = Literal <$> naturalParser
