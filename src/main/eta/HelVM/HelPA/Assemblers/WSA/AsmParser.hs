{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.WSA.AsmParser where

import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Common.AsmParserUtil

import Control.Applicative
import Data.Attoparsec.Text hiding (I, D)
import Data.Char
import Data.Functor
import Data.Maybe


import qualified Data.Text as T

parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
--instructionListParser = many (skipHorizontalSpace *> instructionParser <* skipSpace <* (maybeOption skipComment)) <* endOfInput
--instructionListParser = many ((many (skipHorizontalSpace *> skipComment)) *> skipHorizontalSpace *> instructionParser <* skipSpace <* (maybeOption skipComment)) <* endOfInput
instructionListParser = do 
  list <- many maybeInstructionParser
  return $ list >>= maybeToList

maybeInstructionParser :: Parser (Maybe Instruction)
maybeInstructionParser =
      (fmap Just $ skipSpace *> instructionParser <* skipHorizontalSpace <* (maybeOption skipComment))
  <|> skipSpace *> skipComment *> pure Nothing

----

instructionParser :: Parser Instruction
instructionParser =
  try pushSParser
  <|> maybeOperandInstructionParser
  <|> identifierOperandInstructionParser
  <|> naturalOperandInstructionParser
  <|> zeroOperandInstructionParser

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser =
      zeroOperandInstruction Pop "pop"
  <|> zeroOperandInstruction Doub "doub"
  <|> zeroOperandInstruction Swap "swap"
  <|> zeroOperandInstruction Ret "ret"
  <|> zeroOperandInstruction Exit "exit"
  <|> zeroOperandInstruction OutN "outn"
  <|> zeroOperandInstruction OutC "outc"
  <|> zeroOperandInstruction InN "inn"
  <|> zeroOperandInstruction InC "inc"
    where zeroOperandInstruction i t = asciiCI t *> endWordParser Data.Functor.$>  i

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser =
      maybeOperandInstruction Add "add"
  <|> maybeOperandInstruction Sub "sub"
  <|> maybeOperandInstruction Mul "mul"
  <|> maybeOperandInstruction Div "div"
  <|> maybeOperandInstruction Mod "mod"
  <|> maybeOperandInstruction Store "store"
  <|> maybeOperandInstruction Retrieve "retrive"
    where maybeOperandInstruction f t = fmap f $ asciiCI t *> maybeSkip1HorizontalSpaceAndNaturalParser

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser = 
      identifierOperandInstruction Mark "label"
  <|> identifierOperandInstruction Call "call"
  <|> identifierOperandInstruction Jump "jump"
  <|> identifierOperandInstruction JumpZ "jumpZ"
  <|> identifierOperandInstruction JumpN "jumpN"
  <|> identifierOperandInstruction JumpP "jumpP"
  <|> identifierOperandInstruction JumpNZ "jumpNZ"
  <|> identifierOperandInstruction JumpPZ "jumpPZ"
  <|> identifierOperandInstruction Include "include"
    where identifierOperandInstruction f t = fmap f $ asciiCI t *> skip1HorizontalSpace *> identifierParser

naturalOperandInstructionParser :: Parser Instruction
naturalOperandInstructionParser =
      naturalOperandInstruction Push "push"
  <|> naturalOperandInstruction Test "test"
    where naturalOperandInstruction f t = fmap f $ asciiCI t *> skipHorizontalSpace *> naturalParser

pushSParser :: Parser Instruction
pushSParser = fmap PushS $ asciiCI "pushs" *> skipHorizontalSpace *> stringParser

----

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

endWordParser :: Parser T.Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (c == commentChar)

commentChar :: Char
commentChar = ';'
