module HelVM.HelPA.Common.AsmParserUtil where

import HelVM.HelPA.Common.OrError
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

ordCharLiteralParser :: Integral a => Parser a
ordCharLiteralParser = fromIntegral . ord <$> (char '\'' *> anyChar)

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

fileNameParser :: Parser Identifier
fileNameParser = do
  c <- letter
  s <- many alphaNumDot_
  return $ c:s

alphaNum_ :: Parser Char
alphaNum_ = satisfy isAlphaNum_

alphaNumDot_ :: Parser Char
alphaNumDot_ = satisfy isAlphaNumDot_

skipAllToEndOfLine :: Parser ()
skipAllToEndOfLine = skipWhile isNotEndOfLine

----

asciiCIChoices :: [T.Text] -> Parser T.Text
asciiCIChoices = choice . map asciiCI

isNotEndOfLine :: Char -> Bool
isNotEndOfLine c = not $ isEndOfLine c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || '_' == c

isAlphaNumDot_ :: Char -> Bool
isAlphaNumDot_ c = isAlphaNum c || '_' == c || '.' == c
