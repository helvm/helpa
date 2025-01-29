module HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           HelVM.HelIO.CartesianProduct

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
zeroOperandInstructionParser = choiceMap (uncurry parser)
  [ Pop        >< "pop"
  , Dup        >< "doub"
  , Swap       >< "swap"
  , Return     >< "ret"
  , End        >< "exit"
  , OutputNum  >< "outn"
  , OutputChar >< "outc"
  , InputNum   >< "inn"
  , InputChar  >< "inc"
  ] where parser i t = i <$ (asciiCI t *> endWordParser)

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser = choiceMap (uncurry parser)
  [ Add   >< "add"
  , Sub   >< "sub"
  , Mul   >< "mul"
  , Div   >< "div"
  , Mod   >< "mod"
  , Store >< "store"
  , Load  >< "retrive"
  ] where parser f t = f <$> (asciiCI t *> optional (skip1HorizontalSpace *> integerParser))

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser = choiceMap (uncurry parser)
  [ Mark     >< "label"
  , Call     >< "call"
  , Branch   >< "jump"
  , BranchZ  >< "jumpZ"
  , BranchM  >< "jumpN"
  , BranchP  >< "jumpP"
  , BranchNP >< "jumpNZ"
  , BranchNM >< "jumpPZ"
  , BranchNZ >< "jumpNP"
  , BranchNZ >< "jumpPN"
  , Include  >< "include"
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
