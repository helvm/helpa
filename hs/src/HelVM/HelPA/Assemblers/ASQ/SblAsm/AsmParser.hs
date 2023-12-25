module HelVM.HelPA.Assemblers.ASQ.SblAsm.AsmParser where

import           HelVM.HelPA.Assemblers.ASQ.SblAsm.Instruction

import           HelVM.HelPA.Assembler.Lexer

--import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.Attoparsec.Text



--labelParser2 :: Parser Identifier
--labelParser2 = labelParser


directiveParser :: Parser Instruction
directiveParser = Directive <$> (wordParser <|> equParser)


wordParser :: Parser Directive
wordParser = Word <$> ((string ".word" <* skipHorizontalSpace) *> manyNonEmpty (integerLiteralParser <* skipHorizontalSpace))

equParser :: Parser Directive
equParser = liftA2 Equ ((string ".equ" <* skipHorizontalSpace) *> (identifierParser <* skipHorizontalSpace)) (integerLiteralParser <* skipHorizontalSpace)



----

manyNonEmpty :: (Monad f, Alternative f) => f a -> f $ NonEmpty a
manyNonEmpty p = liftA2 (:|) p $ many p
