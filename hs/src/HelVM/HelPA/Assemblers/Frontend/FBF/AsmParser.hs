module HelVM.HelPA.Assemblers.Frontend.FBF.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.FBF.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Atto.Extra
import           HelVM.HelPA.Assembler.AsmParser.Atto.Parsers
import           HelVM.HelPA.Assembler.AsmParser.Atto.ValueParsers

import           HelVM.HelPA.Assembler.Macro
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.Attoparsec.Text
import           Data.Char                                         hiding (Space)
import           Data.Function.Flip

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = parseWholeText commentSign instructionParser

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
zeroOperandCompilerParser = choiceMap parser
  [ Echo      >< "#echo"
  , ByteCells >< "#bytecells"
  , EndBlock  >< "#endblock"
  ] where parser = uncurry $ zeroOperandParser endWordParser

naturalOperandCompilerParser :: Parser CompilerInstruction
naturalOperandCompilerParser = choiceMap parser
  [ LineBreaks >< "#linebreaks"
  , Custom     >< "#custom"
  ] where parser (f , t) = mapParser f t $ skipHorizontalSpace *> naturalParser

lineBreaksParser :: Parser CompilerInstruction
lineBreaksParser = mapParser LineBreaks "#linebreaks" $ skipHorizontalSpace *> naturalParser

tableParser :: Parser CompilerInstruction
tableParser = mapParser (flip Table) "#table"
      (skipHorizontalSpace *> identifierParser)
  <*> (skipHorizontalSpace *> naturalParser)

dimParser :: Parser CompilerInstruction
dimParser = mapParser Dim "#dim" $ skipHorizontalSpace *> identifiers1Parser

--

zeroOperandCodeParser :: Parser CodeInstruction
zeroOperandCodeParser = choiceMap parser
  [ Bell  >< "bell"
  , Line  >< "line"
  , Space >< "space"
  , Tab   >< "tab"
  , End   >< "end"
  , Rem   >< "rem"
  ] where parser = uncurry $ zeroOperandParser endWordParser

wordOperandCodeParser :: Parser CodeInstruction
wordOperandCodeParser = mapParser BrainFuck "brainfuck" $ skip1HorizontalSpace *> wordParser

identifierCodeParser :: Parser CodeInstruction
identifierCodeParser = choiceMap parser
  [ Read       >< "read"
  , ClearStack >< "cleanstack"
  ] where parser (f , t) = mapParser f t $ skip1HorizontalSpace *> identifierParser

integerValueCodeParser :: Parser CodeInstruction
integerValueCodeParser = mapParser MoveFrom "movefrom" $ skip1HorizontalSpace *> integerValueParser2

identifier2CodeParser :: Parser CodeInstruction
identifier2CodeParser = choiceMap parser
  [ Copy     >< "copy"
  , CopySize >< "copysize"
  , Pop      >< "pop"
  ] where
      parser (f , t) = mapParser f t
            (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)

integerValueIdentifierCodeParser :: Parser CodeInstruction
integerValueIdentifierCodeParser = mapParser Push "push"
            (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

integerIdentifierCodeParser :: Parser CodeInstruction
integerIdentifierCodeParser = choiceMap parser
  [ Inc >< "inc"
  , Dec >< "dec"
  , Set >< "set"
  ] where
      parser (f , t) = mapParser (flip f) t
            (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> integerParser2)

rTableCodeParser :: Parser CodeInstruction
rTableCodeParser = mapParser (flip RTable) "rtable"
      (skip1HorizontalSpace *> identifierParser)
  <*> (skip1HorizontalSpace *> integerValueParser2)
  <*> (skip1HorizontalSpace *> identifierParser)

wTableCodeParser :: Parser CodeInstruction
wTableCodeParser = mapParser (flip3 WTable) "wtable"
      (skip1HorizontalSpace *> identifierParser)
  <*> (skip1HorizontalSpace *> integerValueParser2)
  <*> (skip1HorizontalSpace *> integerValueParser2)

integerValue2IdentifierCodeParser :: Parser CodeInstruction
integerValue2IdentifierCodeParser = choiceMap parser
  [ Add   >< "add"
  , Sub   >< "sub"
  , Multi >< "multi"
  , Mod   >< "mod"
  , Div   >< "div"
  , Comp  >< "comp"
  ] where
      parser (f , t) = mapParser f t
            (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

eqCodeParser :: Parser CodeInstruction
eqCodeParser = choiceMap parser
  [ UnEq    >< "uneq"
  , IfEq    >< "ifeq"
  , IfNotEq >< "ifnoteq"
  ] where
      parser (f , t) = mapParser (flip (eqBlock f)) t
            (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> integerValueParser2)
--        <*> (instructionListParser <* skipSpace <* asciiCI "end")

eqBlock :: (IntegerValue -> Identifier -> InstructionList -> CodeInstruction) -> IntegerValue ->  Identifier -> CodeInstruction
eqBlock f a b = f a b []

byte2AsciiCodeParser :: Parser CodeInstruction
byte2AsciiCodeParser = mapParser Byte2Ascii "byte2ascii"
            (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)
        <*> (skip1HorizontalSpace *> identifierParser)

ascii2ByteCodeParser :: Parser CodeInstruction
ascii2ByteCodeParser = mapParser Ascii2Byte "ascii2byte"
            (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> integerValueParser2)
        <*> (skip1HorizontalSpace *> identifierParser)

msgParser :: Parser CodeInstruction
msgParser = mapParser Msg "msg" $ skipHorizontalSpace *> wordsParser

printParser :: Parser CodeInstruction
printParser = mapParser Print "print" $ skipHorizontalSpace *> identifiers1Parser

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

endWordParser :: Parser ()
endWordParser = void $ takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (commentChar == c)

commentChar :: Char
commentChar = '-'
