module HelVM.HelPA.Assemblers.EAS.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
  naturalToDigitText,
  naturalToDigitString
) where

import           HelVM.HelPA.Assemblers.EAS.Instruction
import           HelVM.HelPA.Assemblers.EAS.Reducer

import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Containers.MTIndexSafe
import           HelVM.Common.Digit.Digits
import           HelVM.Common.Safe

reduceAndGenerateCode :: MonadSafeError m => InstructionList -> m Text
reduceAndGenerateCode il = generateCode =<< reduce il

generateCode :: MonadSafeError m => InstructionList -> m Text
generateCode il = mconcat <$> traverse generateCodeForInstruction il

generateCodeForInstruction :: MonadSafeError m => Instruction -> m Text
generateCodeForInstruction (N (Literal  n)) = generateNatural <$> naturalToDigitText n where generateNatural t = "N" <> t <> "e"
generateCodeForInstruction (N (Variable i)) = liftError $ show i
generateCodeForInstruction (D i)            = liftError $ show i
generateCodeForInstruction (U i)            = liftError $ show i
generateCodeForInstruction (L _)            = pure ""
generateCodeForInstruction R                = pure "\n"
generateCodeForInstruction i                = pure $ show i

naturalToDigitText :: MonadSafeError m => Natural -> m Text
naturalToDigitText value = toText <$> naturalToDigitString value

naturalToDigitString :: MonadSafeError m => Natural -> m String
naturalToDigitString value = traverse naturalToDigitChar $ naturalToDigits7 value

naturalToDigitChar :: MonadSafeError m => Natural -> m Char
naturalToDigitChar i = liftSafe $ ['h', 't', 'a', 'o', 'i', 'n', 's'] `naturalIndexSafe` i
