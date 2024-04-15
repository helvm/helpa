module HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.AsmParser (
  parseAssemblyText,
) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.Instruction

import           HelVM.HelPA.Assemblers.Backend.ASQ.Util.AsmParser

import           HelVM.HelPA.Assembler.AsmParserExtra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.Attoparsec.Text

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText = liftEitherLegacy . parseOnly (instructionListParser <* endOfInput)

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace <* skipManyComment)

instructionParser :: Parser Instruction
instructionParser = liftA2 Instruction labelMaybeParser commandMaybeParser <* endLineParser

labelMaybeParser :: Parser $ Maybe Label
labelMaybeParser = optional labelParser

commandMaybeParser :: Parser $ Maybe Command
commandMaybeParser = optional commandParser

commandParser :: Parser Command
commandParser = skipHorizontalSpace *> (dataParser <|> codeParser)

dataParser :: Parser Command
dataParser = stringWithSpaceParser "data" *> (Data <$> signedIntegerValueWithSpaceParser)

codeParser :: Parser Command
codeParser = stringWithSpaceParser "subleq" *> liftA3 Code signedIntegerValueWithSpaceParser signedIntegerValueWithSpaceParser (optional signedIntegerValueWithSpaceParser)

stringWithSpaceParser :: Text -> Parser Text
stringWithSpaceParser s = string s <* skipHorizontalSpace

signedIntegerValueWithSpaceParser :: Parser IntegerValue
signedIntegerValueWithSpaceParser = signedIntegerValueParser <* skipHorizontalSpace
