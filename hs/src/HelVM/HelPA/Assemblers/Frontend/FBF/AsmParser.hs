module HelVM.HelPA.Assemblers.Frontend.FBF.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.FBF.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.HT
import           Control.Type.Operator
import           Data.Attoparsec.Text
import           Data.Char                                       hiding (Space)
import           Data.Function.Flip

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
--parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <*skipSpace <* endOfInput)
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <*skipSpace)

instructionListParser :: Parser InstructionList
instructionListParser = catMaybes <$> many maybeInstructionParser

maybeInstructionParser :: Parser $ Maybe Instruction
maybeInstructionParser =
       Just <$> (skipSpace *> instructionParser)
  <|> (Nothing <$ (skipSpace *> skipComment))

instructionParser :: Parser $ Instruction
instructionParser = (Compiler <$> compilerParser) <|> (Code <$> codeParser)

compilerParser :: Parser CompilerInstruction
compilerParser =
  try zeroOperandCompilerParser
  <|> naturalOperandCompilerParser
  <|> tableParser
  <|> dimParser
  <|> blockCompilerParser

codeParser :: Parser CodeInstruction
codeParser =
  try zeroOperandCodeParser
  <|> wordOperandCodeParser
  <|> identifierCodeParser
  <|> integerValueCodeParser
  <|> integerIdentifierCodeParser
  <|> identifier2CodeParser
  <|> integerValueIdentifierCodeParser
  <|> rTableCodeParser
  <|> wTableCodeParser
  <|> integerValue2IdentifierCodeParser
  <|> eqCodeParser
  <|> byte2AsciiCodeParser
  <|> ascii2ByteCodeParser
  <|> msgParser
  <|> printParser
  <|> callParser

--

zeroOperandCompilerParser :: Parser CompilerInstruction
zeroOperandCompilerParser =
      parser Echo "#echo"
  <|> parser ByteCells "#bytecells"
  <|> parser EndBlock "#endblock"
    where parser i t = i <$ (asciiCI t *> endWordParser)

naturalOperandCompilerParser :: Parser CompilerInstruction
naturalOperandCompilerParser =
      parser LineBreaks "#linebreaks"
  <|> parser Custom "#custom"
    where parser f t = f <$> (asciiCI t *> skipHorizontalSpace *> naturalParser)

lineBreaksParser :: Parser CompilerInstruction
lineBreaksParser = LineBreaks <$> (asciiCI "#linebreaks" *> skipHorizontalSpace *> naturalParser)

tableParser :: Parser CompilerInstruction
tableParser = lift2 (flip Table) (asciiCI "#table" *> skipHorizontalSpace *> identifierParser) (skipHorizontalSpace *> naturalParser)

dimParser :: Parser CompilerInstruction
dimParser = Dim <$> (asciiCI "#dim" *> skipHorizontalSpace *> identifiers1Parser)

blockCompilerParser :: Parser CompilerInstruction
blockCompilerParser = lift2 block a b where
  a = asciiCI "#block" *> skipHorizontalSpace *> identifierParser
  b = skipHorizontalSpace *> identifiersParser
--  c = instructionListParser
    -- <* skipSpace <* asciiCI "#endblock"

block :: Identifier ->  [Identifier] -> CompilerInstruction
block a b = Block a b []

--

zeroOperandCodeParser :: Parser CodeInstruction
zeroOperandCodeParser =
      parser Bell "bell"
  <|> parser Line "line"
  <|> parser Space "space"
  <|> parser Tab "tab"
  <|> parser End "end"
  <|> parser Rem "rem"
    where parser i t = i <$ (asciiCI t *> endWordParser)

wordOperandCodeParser :: Parser CodeInstruction
wordOperandCodeParser =
      parser BrainFuck "brainfuck"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> wordParser)

identifierCodeParser :: Parser CodeInstruction
identifierCodeParser =
      parser Read "read"
  <|> parser ClearStack "cleanstack"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)

integerValueCodeParser :: Parser CodeInstruction
integerValueCodeParser =
      parser MoveFrom "movefrom"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)

identifier2CodeParser :: Parser CodeInstruction
identifier2CodeParser =
      parser Copy "copy"
  <|> parser CopySize "copysize"
  <|> parser Pop "pop"
    where parser f t = lift2 f (asciiCI t *> skip1HorizontalSpace *> identifierParser) (skip1HorizontalSpace *> identifierParser)

integerValueIdentifierCodeParser :: Parser CodeInstruction
integerValueIdentifierCodeParser =
      parser Push "push"
    where parser f t = lift2 f (asciiCI t *> skip1HorizontalSpace *> integerValueParser2) (skip1HorizontalSpace *> identifierParser)

integerIdentifierCodeParser :: Parser CodeInstruction
integerIdentifierCodeParser =
      parser Inc "inc"
  <|> parser Dec "dec"
  <|> parser Set "set"
    where
      parser f t = lift2 (flip f) (a t) b
      a t = asciiCI t *> skip1HorizontalSpace *> identifierParser
      b = skip1HorizontalSpace *> integerParser2

rTableCodeParser :: Parser CodeInstruction
rTableCodeParser = lift3 (flip RTable) a b c where
  a = asciiCI "rtable" *> skip1HorizontalSpace *> identifierParser
  b = skip1HorizontalSpace *> integerValueParser2
  c = skip1HorizontalSpace *> identifierParser

wTableCodeParser :: Parser CodeInstruction
wTableCodeParser = lift3 (flip3 WTable) a b c where
  a = asciiCI "wtable" *> skip1HorizontalSpace *> identifierParser
  b = skip1HorizontalSpace *> integerValueParser2
  c = skip1HorizontalSpace *> integerValueParser2

integerValue2IdentifierCodeParser :: Parser CodeInstruction
integerValue2IdentifierCodeParser =
      parser Add "add"
  <|> parser Sub "sub"
  <|> parser Multi "multi"
  <|> parser Mod "mod"
  <|> parser Div "div"
  <|> parser Comp "comp"
    where
      parser f t = lift3 f (a t) b c
      a t = asciiCI t *> skip1HorizontalSpace *> integerValueParser2
      b = skip1HorizontalSpace *> integerValueParser2
      c = skip1HorizontalSpace *> identifierParser

eqCodeParser :: Parser CodeInstruction
eqCodeParser =
      parser UnEq "uneq"
  <|> parser IfEq "ifeq"
  <|> parser IfNotEq "ifnoteq"
    where
      parser f t = lift2 (flip (eqBlock f)) (a t) b
      a t = asciiCI t *> skip1HorizontalSpace *> identifierParser
      b = skip1HorizontalSpace *> integerValueParser2
--      c = instructionListParser
       -- <* skipSpace <* asciiCI "end"

eqBlock :: (IntegerValue -> Identifier -> InstructionList -> CodeInstruction) -> IntegerValue ->  Identifier -> CodeInstruction
eqBlock f a b = f a b []

byte2AsciiCodeParser :: Parser CodeInstruction
byte2AsciiCodeParser =
      parser Byte2Ascii "byte2ascii"
  <|> parser Byte2Ascii "BYTE2ASCII"
    where
      parser f t = lift4 f (a t) b c d
      a t = asciiCI t *> skip1HorizontalSpace *> integerValueParser2
      b = skip1HorizontalSpace *> identifierParser
      c = skip1HorizontalSpace *> identifierParser
      d = skip1HorizontalSpace *> identifierParser

ascii2ByteCodeParser :: Parser CodeInstruction
ascii2ByteCodeParser =
      parser Ascii2Byte "ascii2byte"
  <|> parser Ascii2Byte "ASCII2BYTE"
    where
      parser f t = lift4 f (a t) b c d
      a t = asciiCI t *> skip1HorizontalSpace *> integerValueParser2
      b = skip1HorizontalSpace *> integerValueParser2
      c = skip1HorizontalSpace *> integerValueParser2
      d = skip1HorizontalSpace *> identifierParser

msgParser :: Parser CodeInstruction
msgParser = Msg <$> (asciiCI "msg" *> skipHorizontalSpace *> wordsParser)

printParser :: Parser CodeInstruction
printParser = Print <$> (asciiCI "print" *> skipHorizontalSpace *> identifiers1Parser)


callParser :: Parser CodeInstruction
callParser = lift2 Call identifierParser (skipHorizontalSpace *> integerValuesParser)

--

identifiers1Parser :: Parser [Identifier]
identifiers1Parser = sepBy1 identifierParser skipHorizontalSpace <* endOfLine

identifiersParser :: Parser [Identifier]
identifiersParser = sepBy identifierParser skipHorizontalSpace <* endOfLine

integerValuesParser :: Parser [IntegerValue]
integerValuesParser = sepBy integerValueParser2 skipHorizontalSpace <* endOfLine

wordsParser :: Parser [Text]
wordsParser = sepBy1 wordParser skipHorizontalSpace <* endOfLine

wordParser :: Parser Text
wordParser = takeWhile1 (not . isSpace)

--

skipComment :: Parser ()
skipComment = char commentChar *> char commentChar *> skipAllToEndOfLine

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = '-'
