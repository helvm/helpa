module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Mega

import           HelVM.HelIO.CartesianProduct

import           HelVM.HelIO.Control.Safe

import           Text.Megaparsec                                  hiding (Label, many)
import qualified Text.Megaparsec.Char.Lexer                       as L

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = either (liftError . show) pure . runParser instructionListParser "assembly"

instructionListParser :: Parser InstructionList
instructionListParser = spaceConsumer *> many (instructionParser <* spaceConsumer) <* eof

instructionParser :: Parser Instruction
instructionParser =
      Directive <$> directiveParser
  <|> Command   <$> commandParser

directiveParser :: Parser Directive
directiveParser =
      Label       <$> try labelParser
  <|> PushNatural <$> L.decimal
  <|> PushString  <$> stringLiteral
  <|> Print       <$> (symbol "@" *> stringLiteral)
  <|> BranchTable <$> (symbol "bt." *> brackets (branchLabelParser `sepBy` symbol ","))
  <|> Branch      <$> (preBranchParser *> optional branchConditionParser) <*> (symbol "." *> branchLabelParser)
  <|> symbolParser Track ".track"
  <|> symbolParser Halt  "halt"

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
