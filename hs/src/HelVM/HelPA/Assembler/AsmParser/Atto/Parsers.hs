module HelVM.HelPA.Assembler.AsmParser.Atto.Parsers where

import           HelVM.HelPA.Assembler.AsmParser.Extra

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.CartesianProduct
import           HelVM.HelIO.ReadText

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text                  as P

import qualified Data.Text                             as Text

symbolsParser :: Parser () -> [(a, Text)] -> Parser a
symbolsParser sc = choice . (<$>) (uncurry $ symbolParser sc)

symbolParser :: Parser () -> a -> Text -> Parser a
symbolParser sc f = (<$) f . symbol sc

symbols :: Parser () -> [Text] -> Parser Text
symbols sc l = choice $ symbol sc <$> l

--

dotOptLabelParser :: Parser Identifier
dotOptLabelParser = (Text.cons '.' <$> dotLabelParser) <|> labelParser

dotLabelParser :: Parser Identifier
dotLabelParser = char '.' *> identifierParser <* char ':' <* skipHorizontalSpace

labelParser :: Parser Identifier
labelParser = identifierParser <* char ':' <* skipHorizontalSpace

naturalLiteralLabelParser :: Parser Natural
naturalLiteralLabelParser = naturalLiteralParser <* char ':' <* skipHorizontalSpace

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

isNotEndOfLine :: Char -> Bool
isNotEndOfLine = not . isEndOfLine

--

newlineConsumer :: Text -> Parser ()
newlineConsumer = consumer skipNewLine

spaceConsumer :: Text -> Parser ()
spaceConsumer = consumer skipSpace

consumer :: Parser () -> Text -> Parser ()
consumer sp prefix = skipMany $ choice [sp, skipLineComment prefix]

skipLineComment :: Text -> Parser ()
skipLineComment prefix = string prefix *> void (P.takeWhile (not . isNewline))

--

skipNewLine :: Parser ()
skipNewLine = skip isNewline

newline :: Parser Char
newline = satisfy isNewline <?> "newline"

--

symbol :: Parser () -> Text -> Parser Text
symbol spc = lexeme spc . string

lexeme :: Applicative f => f b -> f a -> f a
lexeme spc p = p <* spc

choiceMap :: Alternative f => (a1 -> f a2) -> [a1] -> f a2
choiceMap f l = choice $ f <$> l
