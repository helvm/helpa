module HelVM.HelPA.Assemblers.EAS.CodeGenerator where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Data.List
import Numeric.Natural

generateCode :: InstructionList -> String
generateCode il = show . WhiteInstruction =<< il

newtype WhiteInstruction = WhiteInstruction Instruction

instance Show WhiteInstruction where
  show (WhiteInstruction (N (Literal  n))) = "N" ++ showValue n ++ "e"
  show (WhiteInstruction (N (Variable i))) = error $ show i
  show (WhiteInstruction (D i))            = error $ show i
  show (WhiteInstruction (U i))            = error $ show i
  show (WhiteInstruction (L _))            = ""
  show (WhiteInstruction R)                = "\n"
  show (WhiteInstruction i)                = show i

showValue :: Natural -> String
showValue value = naturalToChar <$> naturalToDigits7 value

naturalToChar :: Natural -> Char
naturalToChar index = ['h', 't', 'a', 'o', 'i', 'n', 's'] `genericIndex` index
