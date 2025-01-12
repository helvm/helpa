module HelVM.HelPA.Assembler.AsmParser.Mega.ValueParsers where

import           HelVM.HelPA.Assembler.AsmParser.Mega.Parsers

import           HelVM.HelPA.Assembler.Value

import           Text.Megaparsec.Char

naturalLabelParser :: Parser NaturalValue
naturalLabelParser =
      Variable <$> (identifierParser <* char ':')
  <|> Literal    <$> (naturalLiteralParser <* char ':')
