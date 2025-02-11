module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Mega

import           HelVM.HelPA.Assembler.Macro
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra                                (many1', showP)

import           Control.Type.Operator

import           Text.Megaparsec                                  hiding (Label, many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer                       as L

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = either (liftError . showP) pure . runParser (instructionListParser <* eof) "assembly"

instructionListParser :: Parser InstructionList
instructionListParser = spaceConsumer *> many (instructionParser <* spaceConsumer)

instructionList1Parser :: Parser $ NonEmpty Instruction
instructionList1Parser = spaceConsumer *> many1' (instructionParser <* spaceConsumer)

instructionParser :: Parser Instruction
instructionParser = choice
  [ Micro <$> microInstructionParser
  , defParser
  , callParser
  ]

--

defParser :: Parser Instruction
defParser = def =<< (symbol "macro" *> instructionList1Parser <* symbol "def")

def :: MonadFail f => NonEmpty Instruction -> f Instruction
def i =  flip (Def []) (init i) <$> checkCall (last i) where
  checkCall :: MonadFail f => Macro a -> f Identifier
  checkCall (Call [] n) = pure n
  checkCall _           = fail "Macro do not have name"

callParser :: Parser Instruction
callParser = try $ call <$> callIdentifier

call :: Identifier -> Instruction
call = Call []

callIdentifier :: Parser Identifier
callIdentifier = checkIdentifier =<< identifierParser

checkIdentifier :: Identifier -> Parser Identifier
checkIdentifier = check <*> flip elem reservedKeywords where
  check i False = pure i
  check i True  = fail $ "Reserved keyword: " <> toString i

reservedKeywords :: [Identifier]
reservedKeywords = ["def"]

--

microInstructionParser :: Parser MicroInstruction
microInstructionParser =
      Directive <$> directiveParser
  <|> Command   <$> commandParser

directiveParser :: Parser Directive
directiveParser =
      Label       <$> try labelParser
  <|> PushInteger <$> integerParser
  <|> PushString  <$> stringLiteral
  <|> Print       <$> (symbol "@" *> stringLiteral)
  <|> BranchTable <$> (string ".btbl" *> branchLabelsParser  <* newline)
  <|> branchParser
  <|> symbolParser Track ".track"
  <|> symbolParser Halt  "halt"

branchLabelsParser :: Parser [BranchLabel]
branchLabelsParser = parseBranchLabels =<< readTillNewline

parseBranchLabels :: MonadFail m => Text -> m [BranchLabel]
parseBranchLabels = either (fail . show) pure . runParser (many (hspace1 *> branchLabelParser)) "branch labels"

readTillNewline :: Parser Text
readTillNewline = toText <$> takeWhileP Nothing (/= '\n')

branchParser :: Parser Directive
branchParser = branch
  <$> (preBranchParser *> optional branchConditionParser)
  <*> (symbol "." *> branchLabelParser)
  <*> optional (symbol "." *>  commandParser)

branch :: Maybe BranchCondition-> BranchLabel -> Maybe Command -> Directive
branch b l c = Branch c b l

preBranchParser :: Parser Text
preBranchParser = try $ symbols ["br" , "b"]

commandParser :: Parser Command
commandParser = symbolsParser
  [ Push   >< "push"
  , Pop    >< "pop"
  , Add    >< "add"
  , Sub    >< "sub"
  , Mul    >< "mul"
  , Div    >< "div"
  , Mod    >< "mod"
  , Not    >< "not"
  , Gt     >< "gt"
  , Ptr    >< "ptr"
  , Switch >< "switch"
  , Dup    >< "dup"
  , Roll   >< "roll"
  , InN    >< "inn"
  , In     >< "in"
  , OutN   >< "outn"
  , Out    >< "out"
  ]

labelParser :: Parser Label
labelParser =
      LIdentifier <$> (identifierParser <* symbol ":")
  <|> LNatural    <$> (L.decimal <* symbol ":")

branchConditionParser :: Parser BranchCondition
branchConditionParser = symbolsParser
  [ BZ  >< "z"
  , BNZ >< "nz"
  , BGT >< "gt"
  , BLE >< "le"
  ]

branchLabelParser :: Parser BranchLabel
branchLabelParser =
      BLIdentifier   <$> identifierParser
  <|> flip BLNatural <$> L.decimal <*> optional branchIdentifierDirectionParser

branchIdentifierDirectionParser :: Parser BranchIdentifierDirection
branchIdentifierDirectionParser = symbolsParser
  [ Backward >< "b"
  , Forward  >< "f"
  ]
