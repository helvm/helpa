module HelVM.HelPA.Assemblers.Frontend.HPL.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.HPL.Instruction

import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Text                                       as T
import           Text.Parser.Combinators                         (between)

parseAssemblyText :: Text -> Either String [Element]
parseAssemblyText = parseOnly fileParser

fileParser :: Parser [Element]
fileParser = many' (elementParser <* skipSpace)

elementParser :: Parser Element
elementParser = choice [sectionParser, asmOrDefParser]


sectionParser :: Parser Element
sectionParser = do
  _ <- string ";;; =========="
  skipSpace
  title <- takeTill (== '=')
  skipWhile (== '=')
  endOfLine
  pure $ Section (T.strip title)

asmOrDefParser :: Parser Element
asmOrDefParser = do
  _ <- choice [string "asm" >> pure True, string "def" >> pure False]
  skipSpace
  inlineFlag <- option  False (string "inline" >> skipSpace >> pure True)
  name <- takeTill isSpace
  skipSpace
  args <- argumentListParser
  skipSpace
  _ <- string "="
  skipSpace
  Asm inlineFlag name args <$> bodyParser

argumentListParser :: Parser [Text]
argumentListParser = between (char '(') (char ')') (sepBy (takeTill (`elem` [',', ')', ' '])) skipSpace)

bodyParser :: Parser Text
bodyParser = do
  _ <- char '('
  body <- takeTill (== ')')
  _ <- char ')'
  pure $ T.strip body
