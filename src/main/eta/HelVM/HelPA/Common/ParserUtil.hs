{-# LANGUAGE TypeFamilies        #-}
module HelVM.HelPA.Common.ParserUtil where

import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Control.Monad.Combinators
import Data.Char
import Data.Functor (void)
import Data.Text (Text)
import Data.Void
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

naturalParser :: Parser Natural
naturalParser = naturalLiteralParser <|> ordCharLiteralParser

naturalLiteralParser :: Parser Natural
naturalLiteralParser = do
  n <- some digitChar
  return (readOrError n::Natural)

ordCharLiteralParser :: Parser Natural
ordCharLiteralParser = do
  c <- char '\'' *> anySingle
  return $ fromIntegral $ ord c

stringParser :: Parser String
stringParser = char '"' *> many (anySingleBut '"') <* char '"'

--skipHorizontalSpace :: Parser ()
skipHorizontalSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
--skipHorizontalSpace = skipWhile isHorizontalSpace
skipHorizontalSpace = void $ takeWhileP (Just "white space") isHorizontalSpace

identifierParser :: Parser Identifier
identifierParser = some letterChar

----

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
