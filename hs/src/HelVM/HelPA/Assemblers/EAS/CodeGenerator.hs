module HelVM.HelPA.Assemblers.EAS.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
  naturalToDigitText,
  naturalToDigitString
) where

import HelVM.HelPA.Assemblers.EAS.Instruction
import HelVM.HelPA.Assemblers.EAS.Reducer

import HelVM.HelPA.Assembler.Value

import HelVM.Common.Digit.Digits
import HelVM.Common.Containers.Lookup
import HelVM.Common.Safe

reduceAndGenerateCode :: InstructionList -> Safe Text
reduceAndGenerateCode il = generateCode <$> reduce il

generateCode :: InstructionList -> Text
generateCode il = mconcat $ unsafe $ sequenceA $ generateCode' <$> il

generateCode' :: Instruction -> Safe Text
generateCode' (N (Literal  n)) = generateNatural <$> naturalToDigitText n where generateNatural t = "N" <> t <> "e"
generateCode' (N (Variable i)) = safeError $ show i
generateCode' (D i)            = safeError $ show i
generateCode' (U i)            = safeError $ show i
generateCode' (L _)            = safe ""
generateCode' R                = safe "\n"
generateCode' i                = safe $ show i

naturalToDigitText :: Natural -> Safe Text
naturalToDigitText value = toText <$> naturalToDigitString value

naturalToDigitString :: Natural -> Safe String
naturalToDigitString value = sequenceA $ naturalToDigitChar <$> naturalToDigits7 value

naturalToDigitChar :: Natural -> Safe Char
naturalToDigitChar i = ['h', 't', 'a', 'o', 'i', 'n', 's'] `naturalIndexSafe` i
