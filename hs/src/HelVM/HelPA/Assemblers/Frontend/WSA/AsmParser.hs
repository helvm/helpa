module HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           HelVM.HelIO.Control.Safe

import           Data.Attoparsec.Text
import           Data.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = parseWholeText commentSign instructionParser

----

instructionParser :: Parser Instruction
instructionParser = choice
  [ pushSParser
  , maybeOperandInstructionParser
  , identifierOperandInstructionParser
  , zeroOperandInstructionParser
  , pushParser
  , testParser
  ]

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser = choice
  [ parser Pop        "pop"
  , parser Dup        "doub"
  , parser Swap       "swap"
  , parser Return     "ret"
  , parser End        "exit"
  , parser OutputNum  "outn"
  , parser OutputChar "outc"
  , parser InputNum   "inn"
  , parser InputChar  "inc"
  ] where parser i t = i <$ (asciiCI t *> endWordParser)

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser = choice
  [ parser Add   "add"
  , parser Sub   "sub"
  , parser Mul   "mul"
  , parser Div   "div"
  , parser Mod   "mod"
  , parser Store "store"
  , parser Load  "retrive"
  ] where parser f t = f <$> (asciiCI t *> optional (skip1HorizontalSpace *> integerParser))

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser = choice
  [ parser Mark     "label"
  , parser Call     "call"
  , parser Branch   "jump"
  , parser BranchZ  "jumpZ"
  , parser BranchM  "jumpN"
  , parser BranchP  "jumpP"
  , parser BranchNP "jumpNZ"
  , parser BranchNM "jumpPZ"
  , parser BranchNZ "jumpNP"
  , parser BranchNZ "jumpPN"
  , parser Include  "include"
  ] where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)

testParser :: Parser Instruction
testParser = Test <$> (asciiCI "test" *> skipHorizontalSpace *> integerParser)

pushParser :: Parser Instruction
pushParser = Push <$> (asciiCI "push" *> skip1HorizontalSpace *> integerParser)

pushSParser :: Parser Instruction
pushSParser = PushS . fromString <$> (asciiCI "pushs" *> skipHorizontalSpace *> stringParser)

----

commentSign :: Parser ()
commentSign = void $ char commentChar

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = ';'
