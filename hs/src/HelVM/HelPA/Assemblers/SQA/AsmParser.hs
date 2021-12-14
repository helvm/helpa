module HelVM.HelPA.Assemblers.SQA.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.SQA.Instruction

import           HelVM.HelPA.Assembler.AsmParserUtil
import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Safe

import           Control.Type.Operator

import           Data.Attoparsec.Text

parseAssemblyText :: MonadSafeError m => Text -> m InstructionList
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <* endOfInput)
--parseAssemblyText = liftEitherLegacy . parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace <* skipManyComment)
--instructionListParser = skipManyComment *> many (skipHorizontalSpace *> instructionParser <* skipHorizontalSpace <* skipManyComment)

instructionParser :: Parser Instruction
instructionParser = liftA2 Instruction labelMaybeParser commandMaybeParser <* endLineParser

labelMaybeParser :: Parser $ Maybe Identifier
labelMaybeParser = optional labelParser

commandMaybeParser :: Parser $ Maybe Command
commandMaybeParser = optional commandParser

commandParser :: Parser Command
commandParser = skipHorizontalSpace *> (dataParser <|> codeParser)

dataParser :: Parser Command
dataParser = stringWithSpaceParser "data" *> (Data <$> signedIntegerValueWithSpaceParser)

codeParser :: Parser Command
codeParser = stringWithSpaceParser "subleq" *> liftA3 Code signedIntegerValueWithSpaceParser signedIntegerValueWithSpaceParser (optional signedIntegerValueWithSpaceParser)

stringWithSpaceParser :: Text -> Parser Text
stringWithSpaceParser s = string s <* skipHorizontalSpace

signedIntegerValueWithSpaceParser :: Parser IntegerValue
signedIntegerValueWithSpaceParser = signedIntegerValueParser <* skipHorizontalSpace

--identifierWithSpaceParser :: Parser Identifier
--identifierWithSpaceParser = identifierParser <* skipHorizontalSpace

----

endLineParser :: Parser Char
endLineParser = char ';' <|> char '\n' <|> skipEndComment

skipEndComment :: Parser Char
skipEndComment = char commentChar <* skipAllToEndOfLine

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = skipHorizontalSpace *> char commentChar *> skipAllToEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')

----

commentChar :: Char
commentChar = '#'
