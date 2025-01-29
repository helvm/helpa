module HelVM.HelPA.Assemblers.Frontend.EIR.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.EIR.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           Data.Attoparsec.Text
import           Data.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
--parseAssemblyText = parseWholeText commentSign instructionParser
parseAssemblyText = parseFirstPartOfText commentSign instructionParser

----

instructionParser :: Parser Instruction
instructionParser = choice
  [ zeroOperandInstructionParser
  , naturalOptInstructionParser
  , identifierOperandInstructionParser
  , integerOperandInstructionParser
  , textOperandInstructionParser
  , labelInstructionParser
  , integerValueInstructionParser
  , integerValueAndIdentifierInstructionParser
  , integerValueAndNaturalValueAndIdentifierInstructionParser
  , pFileInstructionParser
  , pLocInstructionParser
  , markInstructionParser
  ]

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser = choiceMap (uncurry parser)
  [ Exit    >< "exit"
  , Dump    >< "dump"
  , PText   >< ".text"
  ] where parser i t = i <$ (asciiCI t *> endWordParser)

naturalOptInstructionParser :: Parser Instruction
naturalOptInstructionParser =
      parser PData    ".data"
    where
      parser f t = f <$> s where
        s = asciiCI t *> optional (skip1HorizontalSpace *> naturalParser)

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser =
      parser GetC     "getc"
    where
      parser f t = f <$> d where
        d = asciiCI t *> skip1HorizontalSpace *> identifierParser

integerOperandInstructionParser :: Parser Instruction
integerOperandInstructionParser =
      parser PLong    ".long"
    where
      parser f t = f <$> s where
        s = asciiCI t *> skip1HorizontalSpace *> integerParser

textOperandInstructionParser :: Parser Instruction
textOperandInstructionParser =
      parser PString  ".string"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> textParser)

integerValueInstructionParser :: Parser Instruction
integerValueInstructionParser =
      parser PutC     "putc"
    where
      parser f t = f <$> s where
        s = asciiCI t *> (skip1HorizontalSpace *> signedOptIntegerValueParser)

labelInstructionParser :: Parser Instruction
labelInstructionParser =
      parser Jmp      "jmp"
    where
      parser f t = f <$> j where
        j = asciiCI t *> skip1HorizontalSpace *> dotOptIdentifierParser

integerValueAndIdentifierInstructionParser :: Parser Instruction
integerValueAndIdentifierInstructionParser = choiceMap (uncurry parser)
  [ Mov   >< "mov"
  , Add   >< "add"
  , Sub   >< "sub"
  , Load  >< "load"
  , Store >< "store"
  , L CEQ >< "eq"
  , L CNE >< "ne"
  , L CLT >< "lt"
  , L CGT >< "gt"
  , L CLE >< "le"
  , L CGE >< "ge"
  ] where
      parser f t = f
        <$> (asciiCI t *> (skip1HorizontalSpace *> identifierParser))
        <*> (asciiCI "," *> skip1HorizontalSpace *> signedOptIntegerDotOptValueParser)

integerValueAndNaturalValueAndIdentifierInstructionParser :: Parser Instruction
integerValueAndNaturalValueAndIdentifierInstructionParser = choiceMap (uncurry parser)
  [ J CEQ >< "jeq"
  , J CNE >< "jne"
  , J CLT >< "jlt"
  , J CGT >< "jgt"
  , J CLE >< "jle"
  , J CGE >< "jge"
  ] where
      parser f t = f
        <$> (asciiCI t *> (skip1HorizontalSpace *> dotOptIdentifierParser))
        <*> (asciiCI "," *> skip1HorizontalSpace *> identifierParser)
        <*> (asciiCI "," *> skip1HorizontalSpace *> signedOptIntegerValueParser)

pFileInstructionParser :: Parser Instruction
pFileInstructionParser = PFile
  <$> (asciiCI ".file" *> (skip1HorizontalSpace *> naturalParser))
  <*> (skip1HorizontalSpace *> textParser)

pLocInstructionParser :: Parser Instruction
pLocInstructionParser = PLoc
  <$> (asciiCI ".loc" *> (skip1HorizontalSpace *> naturalParser))
  <*> (skip1HorizontalSpace *> naturalParser)
  <*> (skip1HorizontalSpace *> naturalParser)

markInstructionParser :: Parser Instruction
markInstructionParser = Mark <$> dotOptLabelParser

----

commentSign :: Parser ()
commentSign = void $ char commentChar

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = '#'
