{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.WSA.Parser where

import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Common.ParserUtil

import Control.Monad.Combinators
import Data.Functor
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

instance ShowErrorComponent Instruction where
  showErrorComponent = show

--parseAssembler :: String -> Text -> Either Void InstructionList
parseAssembler :: String -> Text -> Either (ParseErrorBundle Text Void) InstructionList
parseAssembler = parse instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = some (space *> instructionParser <* space) <* eof

instructionParser :: Parser Instruction
instructionParser =
      zeroOperandInstructionParser
  <|> identifierOperandInstructionParser
  <|> maybeOperandInstructionParser

zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser = 
      popParser
  <|> doubParser 
  <|> swapParser 
  <|> retParser 
  <|> exitParser 
  <|> outNParser 
  <|> outCParser
  <|> inNParser
  <|> inCParser
    where  
      popParser = string' "pop" Data.Functor.$>  Pop
      doubParser = string' "doub" Data.Functor.$>  Doub
      swapParser = string' "swap" Data.Functor.$>  Swap
      retParser = string' "ret" Data.Functor.$>  Ret
      exitParser = string' "exit" Data.Functor.$>  Exit
      outNParser = string' "outn" Data.Functor.$>  OutN
      outCParser = string' "outc" Data.Functor.$>  OutC
      inNParser = string' "inn" Data.Functor.$>  InN
      inCParser = string' "inc" Data.Functor.$>  InC

identifierOperandInstructionParser :: Parser Instruction
identifierOperandInstructionParser = 
      markParser
  <|> callParser
  <|> jumpParser
    where
      markParser = fmap Mark $ string' "label" *> identifierParser
      callParser = fmap Call $ string' "call" *> identifierParser
      jumpParser = fmap Jump $ string' "jump" *> identifierParser

maybeOperandInstructionParser :: Parser Instruction
maybeOperandInstructionParser =
     storeParser
 <|> retrieveParser
   where
     storeParser = string' "store" Data.Functor.$>  (Store (Just 0))
     retrieveParser = string' "retrive" Data.Functor.$>  (Retrieve (Just 0))
