module HelVM.HelPA.Assemblers.WSA.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
  valueToTL,
  identifierToTL
) where

import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.Token
import HelVM.HelPA.Assemblers.WSA.Reducer

import HelVM.HelPA.Assembler.AssemblyOptions
import HelVM.HelPA.Assembler.TokenType
import HelVM.HelPA.Assembler.Value

import HelVM.Common.Digit.Digitable
import HelVM.Common.Digit.Digits
import HelVM.Common.Safe

reduceAndGenerateCode :: MonadSafeError m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (tokenType options) (startOfInstruction options) (debug options) $ reduce (endOfLine options) il

generateCode :: MonadSafeError m => TokenType -> Bool -> Bool -> InstructionList -> m Text
generateCode tokenType startOfInstruction debug il = showTLByType tokenType <$> generateTL startOfInstruction debug il

generateTL :: MonadSafeError m => Bool -> Bool -> InstructionList -> m TokenList
generateTL startOfInstruction debug il = join <$> sequenceA (generateTLForInstruction startOfInstruction debug <$> il)

generateTLForInstruction :: MonadSafeError m => Bool -> Bool -> Instruction -> m TokenList
generateTLForInstruction _    _     EOL = pure [R]
generateTLForInstruction True debug i   = ([E] <>) <$> generateTLForInstruction' debug i
generateTLForInstruction _    debug i   = generateTLForInstruction' debug i

generateTLForInstruction' :: MonadSafeError m => Bool -> Instruction -> m TokenList
-- Stack instructions
generateTLForInstruction' _ (Push (Literal value)) = ([S,S] <>) <$> valueToTL value
generateTLForInstruction' _  Dup                   = pure [S,N,S]
generateTLForInstruction' _  Swap                  = pure [S,N,T]
generateTLForInstruction' _  Pop                   = pure [S,N,N]
--Arithmetic
generateTLForInstruction' _ (Add Nothing)          = pure [T,S,S,S]
generateTLForInstruction' _ (Sub Nothing)          = pure [T,S,S,T]
generateTLForInstruction' _ (Mul Nothing)          = pure [T,S,S,N]
generateTLForInstruction' _ (Div Nothing)          = pure [T,S,T,S]
generateTLForInstruction' _ (Mod Nothing)          = pure [T,S,T,T]
-- Heap access
generateTLForInstruction' _ (Store Nothing)        = pure [T,T,S]
generateTLForInstruction' _ (Load  Nothing)        = pure [T,T,T]
-- Control
generateTLForInstruction' _ (Mark    label)        = ([N,S,S] <>) <$> identifierToTL label
generateTLForInstruction' _ (Call    label)        = ([N,S,T] <> ) <$> identifierToTL label
generateTLForInstruction' _ (Branch  label)        = ([N,S,N] <> ) <$> identifierToTL label
generateTLForInstruction' _ (BranchZ label)        = ([N,T,S] <> ) <$> identifierToTL label
generateTLForInstruction' _ (BranchM label)        = ([N,T,T] <> ) <$>identifierToTL label
generateTLForInstruction' _  Return                = pure [N,T,N]
generateTLForInstruction' _  End                   = pure [N,N,N]
-- IO instructions
generateTLForInstruction' _  OutputChar            = pure [T,N,S,S]
generateTLForInstruction' _  OutputNum             = pure [T,N,S,T]
generateTLForInstruction' _  InputChar             = pure [T,N,T,S]
generateTLForInstruction' _  InputNum              = pure [T,N,T,T]
-- Other instructions
generateTLForInstruction' _ Noop                   = pure []
generateTLForInstruction' True  DebugPrintStack    = pure [N,N,S,S,S]
generateTLForInstruction' True  DebugPrintHeap     = pure [N,N,S,S,T]
generateTLForInstruction' False DebugPrintStack    = pure []
generateTLForInstruction' False DebugPrintHeap     = pure []
generateTLForInstruction' _ i = liftErrorTuple ("Can not handle instruction" , show i)

valueToTL :: MonadSafeError m => Integer -> m TokenList
valueToTL value = integerToTL value <&> (<> [N])

integerToTL :: MonadSafeError m => Integer -> m TokenList
integerToTL value
  | 0 <= value = (S : ) <$> naturalToTL (fromIntegral value)
  | otherwise  = (T : ) <$> naturalToTL (fromIntegral (- value))

naturalToTL :: MonadSafeError m => Natural -> m TokenList
naturalToTL v = liftSafe $ sequenceA $ fromDigit <$> naturalToDigits2 v

identifierToTL :: MonadSafeError m => Identifier -> m TokenList
identifierToTL v = (join <$> sequenceA (charToTL <$> unwrapIdentifier v)) <&> (<> [N])

charToTL :: MonadSafeError m => Char -> m TokenList
charToTL v = sequenceA $ fromDigit <$> toBits8 (ord v `mod` 256)

toBits8 :: Int -> [Natural]
toBits8 = toBitsBySize 8

toBitsBySize :: Int -> Int -> [Natural]
toBitsBySize  0 _ = []
toBitsBySize size 0 = [0 | _ <- [1..size]]
toBitsBySize size x
 | k == 0    = 0 : toBitsBySize size' x
 | otherwise = 1 : toBitsBySize size' (x - k*m)
    where
      size' = size - 1
      m = 2 ^ size'
      k = x `div` m
