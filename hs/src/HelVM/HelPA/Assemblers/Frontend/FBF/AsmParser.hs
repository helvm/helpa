module HelVM.HelPA.Assemblers.Frontend.FBF.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.FBF.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto
import           HelVM.HelPA.Assembler.Macro
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
instructionParser = choice
  [ Micro <$> microInstructionParser
  , blockCompilerParser
  , callParser
  ]

microInstructionParser :: Parser $ MicroInstruction
microInstructionParser = (Compiler <$> compilerParser) <|> (Code <$> codeParser)

compilerParser :: Parser CompilerInstruction
compilerParser =
  try zeroOperandCompilerParser
  <|> naturalOperandCompilerParser
  <|> tableParser
  <|> dimParser

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

--

blockCompilerParser :: Parser $ Instruction
blockCompilerParser = lift2 block a b where
  a = asciiCI "#block" *> skipHorizontalSpace *> identifierParser
  b = skipHorizontalSpace *> identifiersParser
--  c = instructionListParser
    -- <* skipSpace <* asciiCI "#endblock"

block :: Identifier ->  [Identifier] -> Instruction
block n ps = Def ps n []

callParser :: Parser $ Instruction
callParser = lift2 call identifierParser (skipHorizontalSpace *> integerValuesParser)

call :: Identifier -> [IntegerValue] -> Instruction
call n ps = Call ps n

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
rTableCodeParser = flip RTable
  <$> (asciiCI "rtable" *> skip1HorizontalSpace *> identifierParser)
  <*> (skip1HorizontalSpace *> integerValueParser2)
  <*> (skip1HorizontalSpace *> identifierParser)

wTableCodeParser :: Parser CodeInstruction
wTableCodeParser = flip3 WTable
  <$> (asciiCI "wtable" *> skip1HorizontalSpace *> identifierParser)
  <*> (skip1HorizontalSpace *> integerValueParser2)
  <*> (skip1HorizontalSpace *> integerValueParser2)

integerValue2IdentifierCodeParser :: Parser CodeInstruction
integerValue2IdentifierCodeParser =
      parser Add "add"
  <|> parser Sub "sub"
  <|> parser Multi "multi"
  <|> parser Mod "mod"
  <|> parser Div "div"
  <|> parser Comp "comp"
    where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

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
byte2AsciiCodeParser = parser Byte2Ascii "byte2ascii"
    where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)

ascii2ByteCodeParser :: Parser CodeInstruction
ascii2ByteCodeParser = parser Ascii2Byte "ascii2byte"
    where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

msgParser :: Parser CodeInstruction
msgParser = Msg <$> (asciiCI "msg" *> skipHorizontalSpace *> wordsParser)

printParser :: Parser CodeInstruction
printParser = Print <$> (asciiCI "print" *> skipHorizontalSpace *> identifiers1Parser)

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
