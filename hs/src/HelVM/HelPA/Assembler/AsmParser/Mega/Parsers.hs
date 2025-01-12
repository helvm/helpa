module HelVM.HelPA.Assembler.AsmParser.Mega.Parsers where

import           HelVM.HelPA.Assembler.AsmParser.Extra

import           HelVM.HelPA.Assembler.Value

import           Text.Megaparsec                       hiding (Label, many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer            as L

symbolsParser :: Traversable t => Parser () -> t (a, Text) -> Parser a
symbolsParser sc = choice . (<$>) (uncurry $ symbolParser sc)

symbolParser :: Parser () -> a -> Text -> Parser a
symbolParser sc f = (<$) f . symbol sc

symbols :: Traversable t => Parser () -> t Text -> Parser Text
symbols sc l = choice $ symbol sc <$> l

--

identifierParser :: Parser Identifier
identifierParser = toText <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

textParser :: Parser Text
textParser = toText <$> (char '"' *> manyTill L.charLiteral (char '"'))

--

integerParser :: Parser Integer
integerParser = L.signed hspace L.decimal

naturalLiteralParser :: Parser Natural
naturalLiteralParser = L.decimal

--

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = hspace1

--

newlineConsumer :: Text -> Parser ()
newlineConsumer = consumer skipNewLine

spaceConsumer :: Text -> Parser ()
spaceConsumer = consumer space1

consumer :: Parser () -> Text -> Parser ()
consumer sp prefix = L.space sp (L.skipLineComment prefix) empty

skipNewLine :: Parser ()
skipNewLine = void $ takeWhile1P (Just "newline") isNewline

--

lexeme :: Parser () -> Parser a -> Parser a
lexeme = L.lexeme

symbol :: Parser () -> Text -> Parser Text
symbol = L.symbol'

choiceMap :: Alternative f => (a1 -> f a2) -> [a1] -> f a2
choiceMap f l = choice $ f <$> l

type Parser = Parsec Void Text
