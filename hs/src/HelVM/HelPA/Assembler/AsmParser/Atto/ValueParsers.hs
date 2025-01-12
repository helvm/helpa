module HelVM.HelPA.Assembler.AsmParser.Atto.ValueParsers where

import           HelVM.HelPA.Assembler.AsmParser.Atto.Parsers

import           HelVM.HelPA.Assembler.Value

import           Control.Type.Operator

import           Data.Attoparsec.Text

naturalLabelParser :: Parser NaturalValue
naturalLabelParser = Literal <$> naturalLiteralLabelParser <|> Variable <$> labelParser

signedOptIntegerDotOptValueParser :: Parser IntegerValue
signedOptIntegerDotOptValueParser = Literal <$> signedOptIntegerParser <|> Variable <$> dotOptIdentifierParser

signedOptIntegerValueParser :: Parser IntegerValue
signedOptIntegerValueParser = variableParser signedOptIntegerParser

signedIntegerValueParser :: Parser IntegerValue
signedIntegerValueParser = variableParser signedIntegerParser

integerValueParser2 :: Parser IntegerValue
integerValueParser2 = variableParser integerParser2

naturalValueParser :: Parser NaturalValue
naturalValueParser = variableParser naturalParser

variableParser :: Parser a -> Parser $ Value a
variableParser p = Literal <$> p <|> Variable <$> identifierParser
