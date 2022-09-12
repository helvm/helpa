module HelVM.HelPA.Assembler.AsmParserExtra where

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.ReadText

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char

labelParser :: Parser Identifier
labelParser = identifierParser <* char ':' <* skipHorizontalSpace

integerValueParser2 :: Parser IntegerValue
integerValueParser2 = Literal <$> integerParser2 <|> Variable <$> identifierParser

signedIntegerValueParser :: Parser IntegerValue
signedIntegerValueParser = Literal <$> signedIntegerParser <|> Variable <$> identifierParser

naturalParser :: Parser Natural
naturalParser = naturalLiteralParser <|> ordCharLiteralParser

signedIntegerParser :: Parser Integer
signedIntegerParser = signedIntegerLiteralParser <|> ordCharLiteralParser

integerParser :: Parser Integer
integerParser = integerLiteralParser <|> ordCharLiteralParser

integerParser2 :: Parser Integer
integerParser2 = integerLiteralParser <|> ordCharLiteralParser2

naturalLiteralParser :: Parser Natural
naturalLiteralParser = readUnsafe <$> many1 digit

signedIntegerLiteralParser :: Parser Integer
signedIntegerLiteralParser = signed integerLiteralParser

integerLiteralParser :: Parser Integer
integerLiteralParser = readUnsafe <$> many1 digit

ordCharLiteralParser :: Integral a => Parser a
ordCharLiteralParser = fromIntegral . ord <$> (char '\'' *> anyChar)

ordCharLiteralParser2 :: Integral a => Parser a
ordCharLiteralParser2 = fromIntegral . ord <$> (escapedCharLiteralParser2 <|> charLiteralParser2)

escapedCharLiteralParser2 :: Parser Char
escapedCharLiteralParser2 =
      escape '\'' '\''
  <|> escape '\\' '\\'
  <|> escape '\0' '0'
  <|> escape '\a' 'a'
  <|> escape '\b' 'b'
  <|> escape '\f' 'f'
  <|> escape '\n' 'n'
  <|> escape '\r' 'r'
  <|> escape '\t' 't'
  <|> escape '\v' 'v'

escape :: Char -> Char -> Parser Char
escape a b = a <$ char '\'' *> char '\\' *> char b <* char '\'' <* skipHorizontalSpace

charLiteralParser2 :: Parser Char
charLiteralParser2 = char '\'' *> anyChar <* char '\'' <* skipHorizontalSpace

stringParser :: Parser String
stringParser = char '"' *> many (notChar '"') <* char '"'

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skip1HorizontalSpace :: Parser ()
skip1HorizontalSpace = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

identifierParser :: Parser Identifier
identifierParser = toIdentifier <$> liftA2 (:) letter_ (many alphaNum_)

fileNameParser :: Parser Identifier
fileNameParser = toIdentifier <$> liftA2 (:) letter (many alphaNumDot_)

letter_ :: Parser Char
letter_ = satisfy isAlpha_ <?> "letter_"

alphaNum_ :: Parser Char
alphaNum_ = satisfy isAlphaNum_

alphaNumDot_ :: Parser Char
alphaNumDot_ = satisfy isAlphaNumDot_

skipAllToEndOfLine :: Parser ()
skipAllToEndOfLine = skipWhile isNotEndOfLine

----

asciiCIChoices :: [Text] -> Parser Text
asciiCIChoices = choice . map asciiCI

isNotEndOfLine :: Char -> Bool
isNotEndOfLine = not . isEndOfLine

isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || '_' == c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || '_' == c

isAlphaNumDot_ :: Char -> Bool
isAlphaNumDot_ c = isAlphaNum_ c || '.' == c

isPlusMinus :: Char -> Bool
isPlusMinus c = '+' == c || '-' == c

unEscape :: String -> String
unEscape s = readUnsafe $ "\"" <> s <> "\""
