module HelVM.HelPA.Assemblers.Frontend.EIR.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.EIR.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.HT
import           Control.Type.Operator
import           Data.Attoparsec.Text
import           Data.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
--parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <*skipSpace <* endOfInput)
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <*skipSpace)

instructionListParser :: Parser InstructionList
instructionListParser = catMaybes <$> many maybeInstructionParser

maybeInstructionParser :: Parser $ Maybe Instruction
maybeInstructionParser =
       Just <$> (skipSpace *> instructionParser)
  <|> (Nothing <$ (skipSpace *> skipComment))

----

instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> naturalOptInstructionParser
  <|> identifierOperandInstructionParser
  <|> integerOperandInstructionParser
  <|> textOperandInstructionParser
  <|> labelInstructionParser
  <|> integerValueInstructionParser
  <|> integerValueAndIdentifierInstructionParser
  <|> integerValueAndNaturalValueAndIdentifierInstructionParser
  <|> pFileInstructionParser
  <|> pLocInstructionParser
  <|> markInstructionParser

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser =
      parser Exit     "exit"
  <|> parser Dump     "dump"
  <|> parser PText    ".text"
    where parser i t = i <$ (asciiCI t *> endWordParser)

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
integerValueAndIdentifierInstructionParser =
      parser Mov  "mov"
  <|> parser Add  "add"
  <|> parser Sub  "sub"
  <|> parser Load "load"
  <|> parser Store "store"
  <|> parser (L CEQ) "eq"
  <|> parser (L CNE) "ne"
  <|> parser (L CLT) "lt"
  <|> parser (L CGT) "gt"
  <|> parser (L CLE) "le"
  <|> parser (L CGE) "ge"
    where
      parser f t = lift2 f d s where
        d = asciiCI t *> (skip1HorizontalSpace *> identifierParser)
        s = asciiCI "," *> skip1HorizontalSpace *> signedOptIntegerDotOptValueParser

integerValueAndNaturalValueAndIdentifierInstructionParser :: Parser Instruction
integerValueAndNaturalValueAndIdentifierInstructionParser =
      parser (J CEQ) "jeq"
  <|> parser (J CNE) "jne"
  <|> parser (J CLT) "jlt"
  <|> parser (J CGT) "jgt"
  <|> parser (J CLE) "jle"
  <|> parser (J CGE) "jge"
    where
      parser f t = lift3 f j d s where
        j = asciiCI t *> (skip1HorizontalSpace *> dotOptIdentifierParser)
        d = asciiCI "," *> skip1HorizontalSpace *> identifierParser
        s = asciiCI "," *> skip1HorizontalSpace *> signedOptIntegerValueParser

pFileInstructionParser :: Parser Instruction
pFileInstructionParser = lift2 PFile op1 op2 where
  op1 = asciiCI ".file" *> (skip1HorizontalSpace *> naturalParser)
  op2 = skip1HorizontalSpace *> textParser

pLocInstructionParser :: Parser Instruction
pLocInstructionParser = lift3 PLoc op1 op2 op3 where
  op1 = asciiCI ".loc" *> (skip1HorizontalSpace *> naturalParser)
  op2 = skip1HorizontalSpace *> naturalParser
  op3 = skip1HorizontalSpace *> naturalParser

markInstructionParser :: Parser Instruction
markInstructionParser = Mark <$> dotOptLabelParser

----

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = '#'
