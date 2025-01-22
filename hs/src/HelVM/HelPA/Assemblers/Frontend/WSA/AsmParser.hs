module HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assembler.AsmParserExtra

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator
import           Data.Attoparsec.Text
import           Data.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <*skipSpace <* endOfInput)

instructionListParser :: Parser InstructionList
instructionListParser = catMaybes <$> many maybeInstructionParser

maybeInstructionParser :: Parser $ Maybe Instruction
maybeInstructionParser =
       Just <$> (skipSpace *> instructionParser)
  <|> (Nothing <$ (skipSpace *> skipComment))

----

instructionParser :: Parser Instruction
instructionParser =
      pushSParser
  <|> maybeOperandInstructionParser
  <|> identifierOperandInstructionParser
  <|> zeroOperandInstructionParser
  <|> pushParser
  <|> testParser

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser = choiceMap (zeroOperandParser endWordParser)
  [ (Pop        , "pop" )
  , (Dup        , "doub")
  , (Swap       , "swap")
  , (Return     , "ret" )
  , (End        , "exit")
  , (OutputNum  , "outn")
  , (OutputChar , "outc")
  , (InputNum   , "inn" )
  , (InputChar  , "inc" )
  ]

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser = choiceMap maybeOperandParser
  [ (Add   , "add"    )
  , (Sub   , "sub"    )
  , (Mul   , "mul"    )
  , (Div   , "div"    )
  , (Mod   , "mod"    )
  , (Store , "store"  )
  , (Load  , "retrive")
  ]

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser = choiceMap identifierOperandParser
  [ ( Mark     , "label"  )
  , ( Call     , "call"   )
  , ( Branch   , "jump"   )
  , ( BranchZ  , "jumpZ"  )
  , ( BranchM  , "jumpN"  )
  , ( BranchP  , "jumpP"  )
  , ( BranchNP , "jumpNZ" )
  , ( BranchNM , "jumpPZ" )
  , ( BranchNZ , "jumpNP" )
  , ( BranchNZ , "jumpPN" )
  , ( Include  , "include")
  ]

testParser :: Parser Instruction
testParser = Test <$> (asciiCI "test" *> skipHorizontalSpace *> integerParser)

pushParser :: Parser Instruction
pushParser = Push <$> (asciiCI "push" *> skip1HorizontalSpace *> integerParser)

pushSParser :: Parser Instruction
pushSParser = PushS . fromString <$> (asciiCI "pushs" *> skipHorizontalSpace *> stringParser)

----

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = ';'
