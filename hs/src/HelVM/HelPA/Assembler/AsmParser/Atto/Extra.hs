module HelVM.HelPA.Assembler.AsmParser.Atto.Extra where

import           HelVM.HelPA.Assembler.AsmParser.Atto.Parsers

import           HelVM.HelPA.Assembler.Extra

import           HelVM.HelIO.Control.Safe                     hiding ((<?>))

import           Control.Type.Operator

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text


parseWholeText :: MonadSafe m => Parser () -> Parser a -> Text -> m [a]
parseWholeText commentSign instructionParser = parseText (listParser commentSign instructionParser <* skipSpace <* endOfInput)

parseFirstPartOfText :: MonadSafe m => Parser () -> Parser a -> Text -> m [a]
parseFirstPartOfText commentSign instructionParser = parseText (listParser commentSign instructionParser <* skipSpace)

parseText :: MonadSafe m => Parser a -> Text -> m a
parseText p = liftEitherLegacy . parseOnly p

listParser :: Parser () -> Parser a -> Parser [a]
listParser commentSign instructionParser = catMaybes <$> many (skipSpace *> maybeParser commentSign instructionParser)

maybeParser :: Parser () -> Parser a -> Parser $ Maybe a
maybeParser commentSign instructionParser = choice
  [ Nothing <$ (commentSign *> skipAllToEndOfLine)
  , Just <$> instructionParser
  ]

--

zeroOperandParser :: Parser () -> a -> Text -> Parser a
zeroOperandParser endWordParser i = mapLParser endWordParser (const i)

mapLParser :: Parser a -> (a -> b) -> Text -> Parser b
mapLParser = slipl mapParser

mapParser :: (a -> b) -> Text -> Parser a -> Parser b
mapParser f t p = f <$> (asciiCI t *> p)

