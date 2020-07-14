module HelVM.HelPA.Assemblers.EAS.CodeGenerator where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Numeric.Natural

codeGeneration :: InstructionList -> String
codeGeneration il = (map WhiteInstruction il) >>= show

newtype WhiteInstruction = WhiteInstruction Instruction

instance Show WhiteInstruction where
  show (WhiteInstruction (N (Literal  n))) = "N" ++ (showNumber n) ++ "e"
  show (WhiteInstruction (N (Variable i))) = error $ show i
  show (WhiteInstruction (D i))            = error $ show i
  show (WhiteInstruction (U i))            = error $ show i
  show (WhiteInstruction (L _))            = ""
  show (WhiteInstruction R)                = "\n"
  show (WhiteInstruction i)                = show i

showNumber :: Natural -> String
showNumber n = map naturalToChar $ naturalTo7Digits n

naturalToChar :: Int -> Char
naturalToChar i = ['h', 't', 'a', 'o', 'i', 'n', 's'] !! i
