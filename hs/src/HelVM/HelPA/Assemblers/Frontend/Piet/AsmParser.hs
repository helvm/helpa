module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser where

import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

import           HelVM.HelPA.Assembler.AsmParser.Mega

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
      Macro       <$> (symbol "macro" *> identifierParser) <*> braced instructionListParser
  <|> Label       <$> try labelParser
  <|> PushNatural <$> L.decimal
  <|> PushString <$> stringLiteral
  <|> Print <$> (symbol "@" *> stringLiteral)
  <|> BranchTable <$> (symbol "bt." *> brackets (branchLabelParser `sepBy` symbol ","))
  <|> Branch <$> (preBranchParser *> optional branchConditionParser) <*> (symbol "." *> branchLabelParser)
  <|> symbolParser Track ".track"
  <|> symbolParser Halt  "halt"

preBranchParser :: Parser Text
preBranchParser = try $ symbol "br" <|> symbol "b"

commandParser :: Parser Command
commandParser =
      symbolParser Push   "push"
  <|> symbolParser Pop    "pop"
  <|> symbolParser Add    "add"
  <|> symbolParser Sub    "sub"
  <|> symbolParser Mul    "mul"
  <|> symbolParser Div    "div"
  <|> symbolParser Mod    "mod"
  <|> symbolParser Not    "not"
  <|> symbolParser Gt     "gt"
  <|> symbolParser Ptr    "ptr"
  <|> symbolParser Switch "switch"
  <|> symbolParser Dup    "dup"
  <|> symbolParser Roll   "roll"
  <|> symbolParser InN    "inn"
  <|> symbolParser In     "in"
  <|> symbolParser OutN   "outn"
  <|> symbolParser Out    "out"

labelParser :: Parser Label
labelParser =
      LIdentifier <$> (identifierParser <* symbol ":")
  <|> LNatural <$> (L.decimal <* symbol ":")

branchConditionParser :: Parser BranchCondition
branchConditionParser =
      symbolParser BZ  "z"
  <|> symbolParser BNZ "nz"
  <|> symbolParser BGT "gt"
  <|> symbolParser BLE "le"

branchLabelParser :: Parser BranchLabel
branchLabelParser =
      BLIdentifier   <$> identifierParser
  <|> flip BLNatural <$> L.decimal <*> optional branchIdentifierDirectionParser

branchIdentifierDirectionParser :: Parser BranchIdentifierDirection
branchIdentifierDirectionParser = symbolsParser
  [ (Backward, "b")
  , (Forward , "f")
  ]


