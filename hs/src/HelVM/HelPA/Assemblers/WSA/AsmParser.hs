module HelVM.HelPA.Assemblers.WSA.AsmParser (
  parseAssemblyText
) where

import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assembler.AsmParserUtil
import HelVM.HelPA.Assembler.Value

import HelVM.Common.Safe

import Data.Attoparsec.Text hiding (I, D)
import Data.Char

parseAssemblyText :: Text -> Safe InstructionList
parseAssemblyText code = safeLegacyToSafe $ parseOnly (instructionListParser <*skipSpace <* endOfInput) code

instructionListParser :: Parser InstructionList
instructionListParser = catMaybes <$> many maybeInstructionParser

maybeInstructionParser :: Parser (Maybe Instruction)
maybeInstructionParser =
       Just <$> (skipSpace *> instructionParser)
  <|> (Nothing <$ (skipSpace *> skipComment))

----

instructionParser :: Parser Instruction
instructionParser =
  try pushSParser
  <|> maybeOperandInstructionParser
  <|> identifierOperandInstructionParser
  <|> zeroOperandInstructionParser
  <|> pushParser
  <|> testParser

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser =
      parser Pop        "pop"
  <|> parser Dup        "doub"
  <|> parser Swap       "swap"
  <|> parser Return     "ret"
  <|> parser End        "exit"
  <|> parser OutputNum  "outn"
  <|> parser OutputChar "outc"
  <|> parser InputNum   "inn"
  <|> parser InputChar  "inc"
    where parser i t = i <$ (asciiCI t *> endWordParser)

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser =
      parser Add   "add"
  <|> parser Sub   "sub"
  <|> parser Mul   "mul"
  <|> parser Div   "div"
  <|> parser Mod   "mod"
  <|> parser Store "store"
  <|> parser Load  "retrive"
    where parser f t = f <$> (asciiCI t *> optional (Literal <$> (skip1HorizontalSpace *> integerParser)))

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser =
      parser Mark     "label"
  <|> parser Call     "call"
  <|> parser Branch   "jump"
  <|> parser BranchZ  "jumpZ"
  <|> parser BranchM  "jumpN"
  <|> parser BranchP  "jumpP"
  <|> parser BranchNP "jumpNZ"
  <|> parser BranchNM "jumpPZ"
  <|> parser BranchNZ "jumpNP"
  <|> parser BranchNZ "jumpPN"
  <|> parser Include  "include"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)

testParser :: Parser Instruction
testParser = Test <$> (asciiCI "test" *> skipHorizontalSpace *> integerParser)

pushParser :: Parser Instruction
pushParser = Push . Literal <$> (asciiCI "push" *> skip1HorizontalSpace *> integerParser)

pushSParser :: Parser Instruction
pushSParser = PushS . Literal <$> (asciiCI "pushs" *> skipHorizontalSpace *> stringParser)

----

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = ';'
