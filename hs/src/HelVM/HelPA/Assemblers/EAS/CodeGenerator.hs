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
import           HelVM.Common.Control.Safe
import           HelVM.Common.Digit.Digits

reduceAndGenerateCode :: MonadSafe m => InstructionList -> m Text
reduceAndGenerateCode il = generateCode =<< reduce il

generateCode :: MonadSafe m => InstructionList -> m Text
generateCode il = mconcat <$> traverse generateCodeForInstruction il

generateCodeForInstruction :: MonadSafe m => Instruction -> m Text
generateCodeForInstruction (N (Literal  n)) = generateNatural <$> naturalToDigitText n where generateNatural t = "N" <> t <> "e"
generateCodeForInstruction (N (Variable i)) = liftError $ show i
generateCodeForInstruction (D i)            = liftError $ show i
generateCodeForInstruction (U i)            = liftError $ show i
generateCodeForInstruction (L _)            = pure ""
generateCodeForInstruction R                = pure "\n"
generateCodeForInstruction i                = pure $ show i

naturalToDigitText :: MonadSafe m => Natural -> m Text
naturalToDigitText value = toText <$> naturalToDigitString value

naturalToDigitString :: MonadSafe m => Natural -> m String
naturalToDigitString value = traverse naturalToDigitChar $ naturalToDigits7 value

naturalToDigitChar :: MonadSafe m => Natural -> m Char
naturalToDigitChar i = liftSafe $ ['h', 't', 'a', 'o', 'i', 'n', 's'] `naturalIndexSafe` i
