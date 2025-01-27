module HelVM.HelPA.Assembler.AsmParser.Atto where

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.ReadText

import           Control.Applicative.HT
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text

import           Data.Char
import qualified Data.Text                   as Text

labelParser2 :: Parser NaturalValue
labelParser2 = Literal <$> naturalParser <|> Variable <$> labelParser

signedOptIntegerDotOptValueParser :: Parser IntegerValue
signedOptIntegerDotOptValueParser = Literal <$> signedOptIntegerParser <|> Variable <$> dotOptIdentifierParser

signedOptIntegerValueParser :: Parser IntegerValue
signedOptIntegerValueParser = Literal <$> signedOptIntegerParser <|> Variable <$> identifierParser

signedIntegerValueParser :: Parser IntegerValue
signedIntegerValueParser = Literal <$> signedIntegerParser <|> Variable <$> identifierParser

integerValueParser2 :: Parser IntegerValue
integerValueParser2 = Literal <$> integerParser2 <|> Variable <$> identifierParser

naturalValueParser :: Parser NaturalValue
naturalValueParser = Literal <$> naturalParser <|> Variable <$> identifierParser

dotOptLabelParser :: Parser Identifier
dotOptLabelParser = (Text.cons '.' <$> dotLabelParser) <|> labelParser

dotLabelParser :: Parser Identifier
dotLabelParser = char '.' *> identifierParser <* char ':' <* skipHorizontalSpace

labelParser :: Parser Identifier
labelParser = identifierParser <* char ':' <* skipHorizontalSpace

naturalParser :: Parser Natural
naturalParser = naturalLiteralParser <|> ordCharLiteralParser

signedIntegerParser :: Parser Integer
signedIntegerParser = signedIntegerLiteralParser <|> ordCharLiteralParser

signedOptIntegerParser :: Parser Integer
signedOptIntegerParser = signedOptIntegerLiteralParser <|> ordCharLiteralParser

integerParser :: Parser Integer
integerParser = integerLiteralParser <|> ordCharLiteralParser

integerParser2 :: Parser Integer
integerParser2 = integerLiteralParser <|> ordCharLiteralParser2

naturalLiteralParser :: Parser Natural
naturalLiteralParser = readUnsafe <$> many1 digit

signedOptIntegerLiteralParser :: Parser Integer
signedOptIntegerLiteralParser = signedIntegerLiteralParser <|> integerLiteralParser

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

textParser :: Parser Text
textParser = toText <$> stringParser

stringParser :: Parser String
stringParser = char '"' *> many (notChar '"') <* char '"'

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skip1HorizontalSpace :: Parser ()
skip1HorizontalSpace = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

dotOptIdentifierParser :: Parser Identifier
dotOptIdentifierParser = (Text.cons '.' <$> dotIdentifierParser) <|> identifierParser

dotIdentifierParser :: Parser Identifier
dotIdentifierParser = char '.' *> identifierParser <* skipHorizontalSpace

identifierParser :: Parser Identifier
identifierParser = toIdentifier <$> lift2 (:) letter_ (many alphaNum_)

fileNameParser :: Parser Identifier
fileNameParser = toIdentifier <$> lift2 (:) letter (many alphaNumDot_)

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
