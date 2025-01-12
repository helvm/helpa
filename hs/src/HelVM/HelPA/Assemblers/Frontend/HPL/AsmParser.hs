module HelVM.HelPA.Assemblers.Frontend.HPL.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.HPL.Instruction

import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Text                                       as T
import           Text.Parser.Combinators                         (between)

parseHapyli :: Text -> Either String [Element]
parseHapyli = parseOnly fileParser

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
  return $ Section (T.strip title)

asmOrDefParser :: Parser Element
asmOrDefParser = do
  _ <- choice [string "asm" >> return True, string "def" >> return False] --defType
  skipSpace
  inlineFlag <- option False (string "inline" >> skipSpace >> return True)
  name <- takeTill isSpace
  skipSpace
  args <- argumentListParser
  skipSpace
  _ <-string "="
  skipSpace
  body <- bodyParser
  return $ Asm inlineFlag name args body

argumentListParser :: Parser [Text]
argumentListParser = between (char '(') (char ')') (sepBy (takeTill (`elem` [',', ')', ' '])) skipSpace)

bodyParser :: Parser Text
bodyParser = do
  _ <- char '('
  body <- takeTill (== ')')
  _ <- char ')'
  return $ T.strip body




