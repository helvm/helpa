module HelVM.HelPA.Assembler.AsmParser.Mega where

import           HelVM.HelPA.Assembler.Value

import           Text.Megaparsec             hiding (Label, many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

symbolsParser :: Traversable t => t (a, Text) -> Parser a
symbolsParser = choice . (<$>) (uncurry symbolParser)

symbolParser :: a -> Text -> Parser a
symbolParser f = (<$) f . symbol

identifierParser :: Parser Identifier
identifierParser = toText <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

stringLiteral :: Parser Text
stringLiteral = toText <$> (char '"' *> manyTill L.charLiteral (char '"'))

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

braced :: Parser a -> Parser a
braced = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

type Parser = Parsec Void Text