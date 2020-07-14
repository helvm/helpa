module HelVM.HelPA.Common.AsmParserUtil where

import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text hiding (I, D)
import Data.Char
import Numeric.Natural

import qualified Data.Text as T

naturalParser :: Parser Natural
naturalParser = naturalLiteralParser <|> ordCharLiteralParser

naturalLiteralParser :: Parser Natural
naturalLiteralParser = do
  n <- many1 digit
  return (readOrError n::Natural)

ordCharLiteralParser :: Parser Natural
ordCharLiteralParser = fmap (fromIntegral . ord) (char '\'' *> anyChar)

stringParser :: Parser String
stringParser = char '"' *> many (notChar '"') <* char '"'

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skip1HorizontalSpace :: Parser ()
skip1HorizontalSpace = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

identifierParser :: Parser Identifier
identifierParser = do
  c <- letter
  s <- many alphaNum_
  return $ c:s

alphaNum_ :: Parser Char
alphaNum_ = satisfy isAlphaNum_

skipAllToEndOfLine :: Parser ()
skipAllToEndOfLine = skipWhile isNotEndOfLine

----

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = optional p

maybeNaturalParser :: Parser (Maybe Natural)
maybeNaturalParser = maybeOption naturalParser

maybeSkip1HorizontalSpaceAndNaturalParser :: Parser (Maybe Natural)
maybeSkip1HorizontalSpaceAndNaturalParser = maybeOption (skip1HorizontalSpace *> naturalParser)

----

asciiCIChoices :: [T.Text] -> Parser T.Text
asciiCIChoices = choice . map asciiCI

isNotEndOfLine :: Char -> Bool
isNotEndOfLine c = not $ isEndOfLine c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || '_' == c

