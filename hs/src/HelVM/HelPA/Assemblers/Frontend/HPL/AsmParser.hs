module HelVM.HelPA.Assemblers.Frontend.HPL.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.HPL.Instruction

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T

-- Parser for a section header
sectionParser :: Parser Element
sectionParser = do
  string ";;; =========="
  skipSpace
  title <- takeTill (== '=')
  skipWhile (== '=')
  endOfLine
  return $ Section (T.strip title)

-- Parser for "asm" and "def" definitions
asmOrDefParser :: Parser Element
asmOrDefParser = do
  defType <- choice [string "asm" >> return True, string "def" >> return False]
  skipSpace
  inlineFlag <- option False (string "inline" >> skipSpace >> return True)
  name <- takeTill isSpace
  skipSpace
  args <- argumentListParser
  skipSpace
  string "="
  skipSpace
  body <- bodyParser
  return $ if defType
           then Asm inlineFlag name args body
           else Def inlineFlag name args body

-- Parser for argument list
argumentListParser :: Parser [Text]
argumentListParser = between (char '(') (char ')') (sepBy (takeTill (`elem` [',', ')', ' '])) skipSpace)

-- Parser for a body (could be multiline)
bodyParser :: Parser Text
bodyParser = do
  char '('
  body <- takeTill (== ')')
  char ')'
  return $ T.strip body

-- Wrapper to parse elements
elementParser :: Parser Element
elementParser = choice [sectionParser, asmOrDefParser]

-- Parser for the entire file
fileParser :: Parser [Element]
fileParser = many' (elementParser <* skipSpace)

-- Utility to run the parser
parseHapyli :: Text -> Either String [Element]
parseHapyli = parseOnly fileParser

---- Example usage (for testing)
--exampleInput :: Text
--exampleInput = T.pack """
--;;; ========== ARITHMETIC ==========
--
--asm inline +  (x y) = ( add )
--asm inline ++ (x)   = ( push 1 add )
--
--def inline or (x y) = (- (+ x y) (* x y))
--"""
--
--main :: IO ()
--main = case parseHapyli exampleInput of
--  Left err -> putStrLn $ "Parse error: " ++ err
--  Right result -> mapM_ print result
