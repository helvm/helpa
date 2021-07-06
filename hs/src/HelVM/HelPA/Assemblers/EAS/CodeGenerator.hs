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

reduceAndGenerateCode :: MonadSafeError m => InstructionList -> m Text
reduceAndGenerateCode il = generateCode =<< reduce il

generateCode :: MonadSafeError m => InstructionList -> m Text
generateCode il = mconcat <$> sequenceA (generateCode' <$> il)

generateCode' :: MonadSafeError m => Instruction -> m Text
generateCode' (N (Literal  n)) = generateNatural <$> naturalToDigitText n where generateNatural t = "N" <> t <> "e"
generateCode' (N (Variable i)) = liftError $ show i
generateCode' (D i)            = liftError $ show i
generateCode' (U i)            = liftError $ show i
generateCode' (L _)            = pure ""
generateCode' R                = pure "\n"
generateCode' i                = pure $ show i

naturalToDigitText :: MonadSafeError m => Natural -> m Text
naturalToDigitText value = toText <$> naturalToDigitString value

naturalToDigitString :: MonadSafeError m => Natural -> m String
naturalToDigitString value = sequenceA $ naturalToDigitChar <$> naturalToDigits7 value

naturalToDigitChar :: MonadSafeError m => Natural -> m Char
naturalToDigitChar i = liftSafe $ ['h', 't', 'a', 'o', 'i', 'n', 's'] `naturalIndexSafe` i
