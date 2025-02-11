module HelVM.HelPA.Assembler.AsmParser.Mega where

import           HelVM.HelPA.Assembler.Value

import           Text.Megaparsec             hiding (Label, many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

symbolsParser :: Traversable t => t (a, Text) -> Parser a
symbolsParser = choice . (<$>) (uncurry symbolParser)

symbolParser :: a -> Text -> Parser a
symbolParser f = (<$) f . symbol

symbols :: Traversable t => t Text -> Parser Text
symbols l = choice $ symbol <$> l

--

identifierParser :: Parser Identifier
identifierParser = toText <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

stringLiteral :: Parser Text
stringLiteral = toText <$> (char '"' *> manyTill L.charLiteral (char '"'))

--

integerParser :: Parser Integer
integerParser = L.signed spaceConsumer decimalParser

decimalParser :: Parser Integer
decimalParser = lexeme L.decimal

--

braced :: Parser a -> Parser a
braced = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

--

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

type Parser = Parsec Void Text
