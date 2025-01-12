module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

--import           HelVM.HelPA.Assembler.AsmParser.Atto.Extra
--import           HelVM.HelPA.Assembler.AsmParser.Atto.Parsers
--import           HelVM.HelPA.Assembler.AsmParser.Atto.ValueParsers
import           HelVM.HelPA.Assembler.AsmParser.Mega.Parsers
import           HelVM.HelPA.Assembler.AsmParser.Mega.ValueParsers

import           HelVM.HelPA.Assembler.Macro
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra                                 (many1', showP)

import           Control.Type.Operator

--import           Data.Attoparsec.Text                              hiding (many1')
import           Text.Megaparsec                                   hiding (Label, many)
import           Text.Megaparsec.Char

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = either (liftError . showP) pure . runParser (instructionListParser <* eof) "assembly"

instructionListParser :: Parser InstructionList
instructionListParser = sc *> many (instructionParser <* sc)

instructionList1Parser :: Parser $ NonEmpty Instruction
instructionList1Parser = sc *> many1' (instructionParser <* sc)

instructionParser :: Parser Instruction
instructionParser = choice
  [ Micro <$> microInstructionParser
  , defParser
  , callParser
  ]

--

defParser :: Parser Instruction
defParser = def =<< (symbol sc "macro" *> instructionList1Parser <* symbol sc "def")

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
      Label       <$> try naturalLabelParser
  <|> PushInteger <$> integerParser
  <|> PushString  <$> textParser
  <|> Print       <$> (char '@' *> textParser)
  <|> BranchTable <$> (string ".btbl" *> parseBranchLabels <* nlc <* sc)
  <|> branchParser
  <|> symbolParser sc Track ".track"
  <|> symbolParser sc Halt  "halt"

parseBranchLabels :: Parser [BranchLabel]
parseBranchLabels = many (try (skipHorizontalSpace *> branchLabelParser)) <* optional skipHorizontalSpace

branchParser :: Parser Directive
branchParser = branch
  <$> (preBranchParser *> optional branchConditionParser)
  <*> (char '.' *> branchLabelParser')
  <*> optional (char '.' *>  commandParser)

branch :: Maybe BranchCondition-> BranchLabel -> Maybe Command -> Directive
branch b l c = Branch c b l

preBranchParser :: Parser Text
preBranchParser = try $ symbols sc ["br" , "b"]

commandParser :: Parser Command
commandParser = symbolsParser sc
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

branchConditionParser :: Parser BranchCondition
branchConditionParser = symbolsParser sc
  [ BZ  >< "z"
  , BNZ >< "nz"
  , BGT >< "gt"
  , BLE >< "le"
  ]

branchLabelParser :: Parser BranchLabel
branchLabelParser =
      BLIdentifier   <$> identifierParser
  <|> flip BLNatural <$> naturalLiteralParser <*> optional branchIdentifierDirectionParser

branchIdentifierDirectionParser :: Parser BranchIdentifierDirection
branchIdentifierDirectionParser = choiceMap parser
  [ Backward  >< 'b'
  , Forward   >< 'f'
  ] where parser (f , c) = f <$ char c

branchLabelParser' :: Parser BranchLabel
branchLabelParser' =
      BLIdentifier   <$> identifierParser
  <|> flip BLNatural <$> naturalLiteralParser <*> optional branchIdentifierDirectionParser'

branchIdentifierDirectionParser' :: Parser BranchIdentifierDirection
branchIdentifierDirectionParser' = symbolsParser sc
  [ Backward >< "b"
  , Forward  >< "f"
  ]

--

nlc :: Parser ()
nlc = newlineConsumer commentPrefix

sc :: Parser ()
sc = spaceConsumer commentPrefix

commentPrefix :: Text
commentPrefix = "#"
