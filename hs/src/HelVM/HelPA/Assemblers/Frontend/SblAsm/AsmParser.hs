module HelVM.HelPA.Assemblers.Frontend.SblAsm.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.SblAsm.Instruction


import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Text.Megaparsec                                    hiding (many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer                         as L

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText _ = pure []

parseAssembly :: Text -> Either (ParseErrorBundle Text Void) [Directive]
parseAssembly = runParser (sc *> assemblyFile <* eof) ""

assemblyFile :: Parser [Directive]
assemblyFile = many $ choice
  [ equDef
  , macroDef
  ]

equDef :: Parser Directive
equDef = do
  void $ symbol ".equ"
  name <- lexeme $ takeWhile1P (Just "constant name") (`notElem` (" \t\n" :: String))
  void $ symbol "-"
  value <- lexeme L.decimal
  return $ Equ name value

macroDef :: Parser Directive
macroDef = do
  void $ symbol ".macro"
  name <- lexeme $ takeWhile1P (Just "macro name") (`notElem` (" \t\n" :: String))
  body <- manyTill anySingle (try $ symbol ".endm")
  return $ Macro name (lines $ toText body)

directive :: Parser Text
directive = lexeme $ char '.' >> takeWhile1P (Just "directive") (`notElem` (" \t\n" :: String))

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

type Parser = Parsec Void Text

----

--labelParser2 :: Parser Identifier
--labelParser2 = labelParser

--directiveParser :: Parser Command
--directiveParser = Directive <$> (wordParser <|> equParser)
--
--wordParser :: Parser Directive
--wordParser = Word <$> ((string ".word" <* skipHorizontalSpace) *> manyNonEmpty (integerLiteralParser <* skipHorizontalSpace))
--
--equParser :: Parser Directive
--equParser = liftA2 Equ ((string ".equ" <* skipHorizontalSpace) *> (identifierParser <* skipHorizontalSpace)) (integerLiteralParser <* skipHorizontalSpace)

----

manyNonEmpty :: (Monad f, Alternative f) => f a -> f $ NonEmpty a
manyNonEmpty p = liftA2 (:|) p $ many p
