module HelVM.HelPA.Assembler.AsmParser.Atto where

import           HelVM.HelPA.Assembler.Extra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.ReadText

import           HelVM.HelIO.CartesianProduct
import           HelVM.HelIO.Control.Safe     hiding ((<?>))

import           Control.Type.Operator

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text

import           Data.Char
import qualified Data.Text                    as Text

parseWholeText :: MonadSafe m => Parser () -> Parser a -> Text -> m [a]
parseWholeText commentSign instructionParser = parseText (listParser commentSign instructionParser <* skipSpace <* endOfInput)

parseFirstPartOfText :: MonadSafe m => Parser () -> Parser a -> Text -> m [a]
parseFirstPartOfText commentSign instructionParser = parseText (listParser commentSign instructionParser <* skipSpace)

parseText :: MonadSafe m => Parser a -> Text -> m a
parseText p = liftEitherLegacy . parseOnly p

listParser :: Parser () -> Parser a -> Parser [a]
listParser commentSign instructionParser = catMaybes <$> many (maybeParser commentSign instructionParser)

maybeParser :: Parser () -> Parser a -> Parser $ Maybe a
maybeParser commentSign instructionParser = choice
  [ Nothing <$ (skipSpace *> commentSign *> skipAllToEndOfLine)
  , Just <$> (skipSpace *> instructionParser)
  ]

--

zeroOperandParser :: Parser () -> a -> Text -> Parser a
zeroOperandParser endWordParser i = mapLParser endWordParser (const i)

mapLParser :: Parser a -> (a -> b) -> Text -> Parser b
mapLParser = slipl mapParser

mapParser :: (a -> b) -> Text -> Parser a -> Parser b
mapParser f t p = f <$> (asciiCI t *> p)

--

labelParser2 :: Parser NaturalValue
labelParser2 = Literal <$> naturalParser <|> Variable <$> labelParser

signedOptIntegerDotOptValueParser :: Parser IntegerValue
signedOptIntegerDotOptValueParser = Literal <$> signedOptIntegerParser <|> Variable <$> dotOptIdentifierParser

signedOptIntegerValueParser :: Parser IntegerValue
signedOptIntegerValueParser = variableParser signedOptIntegerParser

signedIntegerValueParser :: Parser IntegerValue
signedIntegerValueParser = variableParser signedIntegerParser

integerValueParser2 :: Parser IntegerValue
integerValueParser2 = variableParser integerParser2

naturalValueParser :: Parser NaturalValue
naturalValueParser = variableParser naturalParser

variableParser :: Parser a -> Parser $ Value a
variableParser p = Literal <$> p <|> Variable <$> identifierParser

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
escapedCharLiteralParser2 = choiceMap (uncurry escape)
  [ '\'' >< '\''
  , '\\' >< '\\'
  , '\0' >< '0'
  , '\a' >< 'a'
  , '\b' >< 'b'
  , '\f' >< 'f'
  , '\n' >< 'n'
  , '\r' >< 'r'
  , '\t' >< 't'
  , '\v' >< 'v'
  ]

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
identifierParser = toIdentifier <$> ((:) <$> letter_ <*> many alphaNum_)

fileNameParser :: Parser Identifier
fileNameParser = toIdentifier <$> ((:) <$> letter <*> many alphaNumDot_)

letter_ :: Parser Char
letter_ = satisfy isAlpha_ <?> "letter_"

alphaNum_ :: Parser Char
alphaNum_ = satisfy isAlphaNum_

alphaNumDot_ :: Parser Char
alphaNumDot_ = satisfy isAlphaNumDot_

skipAllToEndOfLine :: Parser ()
skipAllToEndOfLine = skipWhile isNotEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')

----

asciiCIChoices :: [Text] -> Parser Text
asciiCIChoices = choiceMap asciiCI

choiceMap :: Alternative f => (a1 -> f a2) -> [a1] -> f a2
choiceMap f l = choice $ f <$> l

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
