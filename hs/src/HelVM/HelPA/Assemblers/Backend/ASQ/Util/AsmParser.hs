module HelVM.HelPA.Assemblers.Backend.ASQ.Util.AsmParser where

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           Data.Attoparsec.Text

endLineParser :: Parser Char
endLineParser = char ';' <|> char '\n' <|> skipEndComment

skipEndComment :: Parser Char
skipEndComment = char commentChar <* skipAllToEndOfLine

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = skipHorizontalSpace *> char commentChar *> skipAllToEndOfLine

----

commentChar :: Char
commentChar = '#'
