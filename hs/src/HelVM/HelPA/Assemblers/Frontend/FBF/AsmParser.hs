module HelVM.HelPA.Assemblers.Frontend.FBF.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.FBF.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto
import           HelVM.HelPA.Assembler.Macro
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator
import           Data.Attoparsec.Text
import           Data.Char                                       hiding (Space)
import           Data.Function.Flip

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
--parseAssemblyText = parseWholeText commentSign instructionParser
parseAssemblyText = parseFirstPartOfText commentSign instructionParser

----

instructionParser :: Parser $ Instruction
instructionParser = choice
  [ Micro <$> microInstructionParser
  , blockCompilerParser
  , callParser
  ]

microInstructionParser :: Parser $ MicroInstruction
microInstructionParser = (Compiler <$> compilerParser) <|> (Code <$> codeParser)

compilerParser :: Parser CompilerInstruction
compilerParser = choice
  [ zeroOperandCompilerParser
  , naturalOperandCompilerParser
  , tableParser
  , dimParser
  ]

codeParser :: Parser CodeInstruction
codeParser = choice
  [ zeroOperandCodeParser
  , wordOperandCodeParser
  , identifierCodeParser
  , integerValueCodeParser
  , integerIdentifierCodeParser
  , identifier2CodeParser
  , integerValueIdentifierCodeParser
  , rTableCodeParser
  , wTableCodeParser
  , integerValue2IdentifierCodeParser
  , eqCodeParser
  , byte2AsciiCodeParser
  , ascii2ByteCodeParser
  , msgParser
  , printParser
  ]
--

blockCompilerParser :: Parser $ Instruction
blockCompilerParser = block
  <$> (asciiCI "#block" *> skipHorizontalSpace *> identifierParser)
  <*> (skipHorizontalSpace *> identifiersParser)
--  <*> (instructionListParser <* skipSpace <* asciiCI "#endblock")

block :: Identifier ->  [Identifier] -> Instruction
block n ps = Def ps n []

callParser :: Parser $ Instruction
callParser = call <$> identifierParser <*> (skipHorizontalSpace *> integerValuesParser)

call :: Identifier -> [IntegerValue] -> Instruction
call n ps = Call ps n

--

zeroOperandCompilerParser :: Parser CompilerInstruction
zeroOperandCompilerParser = choiceMap (uncurry parser)
  [ Echo      >< "#echo"
  , ByteCells >< "#bytecells"
  , EndBlock  >< "#endblock"
  ] where parser i t = i <$ (asciiCI t *> endWordParser)

naturalOperandCompilerParser :: Parser CompilerInstruction
naturalOperandCompilerParser = choiceMap (uncurry parser)
  [ LineBreaks >< "#linebreaks"
  , Custom     >< "#custom"
  ] where parser f t = f <$> (asciiCI t *> skipHorizontalSpace *> naturalParser)

lineBreaksParser :: Parser CompilerInstruction
lineBreaksParser = LineBreaks <$> (asciiCI "#linebreaks" *> skipHorizontalSpace *> naturalParser)

tableParser :: Parser CompilerInstruction
tableParser = flip Table
  <$> (asciiCI "#table" *> skipHorizontalSpace *> identifierParser)
  <*> (skipHorizontalSpace *> naturalParser)

dimParser :: Parser CompilerInstruction
dimParser = Dim <$> (asciiCI "#dim" *> skipHorizontalSpace *> identifiers1Parser)

--

zeroOperandCodeParser :: Parser CodeInstruction
zeroOperandCodeParser = choiceMap (uncurry parser)
  [ Bell  >< "bell"
  , Line  >< "line"
  , Space >< "space"
  , Tab   >< "tab"
  , End   >< "end"
  , Rem   >< "rem"
  ] where parser i t = i <$ (asciiCI t *> endWordParser)

wordOperandCodeParser :: Parser CodeInstruction
wordOperandCodeParser =
      parser BrainFuck "brainfuck"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> wordParser)

identifierCodeParser :: Parser CodeInstruction
identifierCodeParser = choiceMap (uncurry parser)
  [ Read       >< "read"
  , ClearStack >< "cleanstack"
  ] where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)

integerValueCodeParser :: Parser CodeInstruction
integerValueCodeParser =
      parser MoveFrom "movefrom"
    where parser f t = f <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)

identifier2CodeParser :: Parser CodeInstruction
identifier2CodeParser = choiceMap (uncurry parser)
  [ Copy     >< "copy"
  , CopySize >< "copysize"
  , Pop      >< "pop"
  ] where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)

integerValueIdentifierCodeParser :: Parser CodeInstruction
integerValueIdentifierCodeParser =
      parser Push "push"
    where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

integerIdentifierCodeParser :: Parser CodeInstruction
integerIdentifierCodeParser = choiceMap (uncurry parser)
  [ Inc >< "inc"
  , Dec >< "dec"
  , Set >< "set"
  ] where
      parser f t = flip f
        <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> integerParser2)

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
integerValue2IdentifierCodeParser = choiceMap (uncurry parser)
  [ Add   >< "add"
  , Sub   >< "sub"
  , Multi >< "multi"
  , Mod   >< "mod"
  , Div   >< "div"
  , Comp  >< "comp"
  ] where
      parser f t = f
        <$> (asciiCI t *> skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

eqCodeParser :: Parser CodeInstruction
eqCodeParser = choiceMap (uncurry parser)
  [ UnEq    >< "uneq"
  , IfEq    >< "ifeq"
  , IfNotEq >< "ifnoteq"
  ] where
      parser f t = flip (eqBlock f)
        <$> (asciiCI t *> skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> integerValueParser2)
--        <*> (instructionListParser <* skipSpace <* asciiCI "end")

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

commentSign :: Parser ()
commentSign = void $ char commentChar *> char commentChar

endWordParser :: Parser Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = '-'
