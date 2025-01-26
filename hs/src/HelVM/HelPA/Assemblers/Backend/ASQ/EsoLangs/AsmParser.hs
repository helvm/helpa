module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assemblers.Backend.ASQ.Util.AsmParser

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.HT
import           Data.Attoparsec.Text

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <* endOfInput)

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace <* skipManyComment)

instructionParser :: Parser Instruction
instructionParser = instructionDataParser <|> instructionCodeParser

instructionDataParser :: Parser Instruction
instructionDataParser = makeDataInstruction <$> (char '.' *> skipHorizontalSpace *> itemListParser)

instructionCodeParser :: Parser Instruction
instructionCodeParser = makeCodeInstruction <$> itemListParser

itemListParser :: Parser ItemList
itemListParser = many itemParser <* endLineParser

itemParser :: Parser Item
itemParser =
      (ItemLabel             <$> labelParser)
  <|> (ItemExpression        <$> expressionParser)
  <|> (ItemString . unEscape <$> stringParser <* skipHorizontalSpace)

expressionParser :: Parser Expression
expressionParser = (termWithPMExpressionParser <|> termWithoutPMExpressionParser) <* skipHorizontalSpace

termWithoutPMExpressionParser :: Parser Expression
termWithoutPMExpressionParser = makeExpressionWithoutPM <$> termParser

termWithPMExpressionParser :: Parser Expression
termWithPMExpressionParser = lift2 (flip makeExpressionWithPM) termParser pmExpressionParser

pmExpressionParser :: Parser PMExpression
pmExpressionParser = lift2 PMExpression pmParser expressionParser

pmParser :: Parser PM
pmParser = (Plus <$ char '+') <|> (Minus <$ char '-')

termParser :: Parser Term
termParser =
      (TermSymbol       <$> integerValueParser2)
  <|> (TermQuestionMark <$  char '?')
  <|> (TermMinus        <$> (char '-' *> termParser))
  <|> (TermExpression   <$> (char '(' *> expressionParser <* char ')'))
