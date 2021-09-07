module HelVM.HelPA.Assemblers.ASQ.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.HelPA.Assembler.AsmParserUtil
import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Safe

import           Data.Attoparsec.Text

parseAssemblyText :: MonadSafeError m => Text -> m InstructionList
--parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <* endOfInput)
parseAssemblyText = liftEitherLegacy . parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace <* skipManyComment)
--instructionListParser = skipManyComment *> many (skipHorizontalSpace *> instructionParser <* skipHorizontalSpace <* skipManyComment)

instructionParser :: Parser Instruction
instructionParser = instructionDataParser <|> instructionCodeParser

instructionDataParser :: Parser Instruction
instructionDataParser = Instruction Data <$> (char '.' *> skipHorizontalSpace *> itemListParser)

instructionCodeParser :: Parser Instruction
instructionCodeParser = Instruction Code <$> itemListParser

itemListParser :: Parser ItemList
itemListParser = many itemParser <* endLineParser

itemParser :: Parser Item
itemParser =
      (ItemLabel             <$> labelParser)
  <|> (ItemExpression        <$> expressionParser)
  <|> (ItemString . unEscape <$> stringParser <* skipHorizontalSpace)

labelParser :: Parser Identifier
labelParser = identifierParser <* char ':' <* skipHorizontalSpace

expressionParser :: Parser Expression
expressionParser = (termWithPMExpressionParser <|> termWithoutPMExpressionParser) <* skipHorizontalSpace

termWithoutPMExpressionParser :: Parser Expression
termWithoutPMExpressionParser = makeExpressionWithoutPM <$> termParser

termWithPMExpressionParser :: Parser Expression
termWithPMExpressionParser = liftA2 (flip makeExpressionWithPM) termParser pmExpressionParser

pmExpressionParser :: Parser PMExpression
pmExpressionParser = liftA2 PMExpression pmParser expressionParser

pmParser :: Parser PM
pmParser = (Plus <$ char '+') <|> (Minus <$ char '-')

termParser :: Parser Term
termParser =
      (TermSymbol       <$> integerValueParser2)
  <|> (TermQuestionMark <$  char '?')
  <|> (TermMinus        <$> (char '-' *> termParser))
  <|> (TermExpression   <$> (char '(' *> expressionParser <* char ')'))

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

