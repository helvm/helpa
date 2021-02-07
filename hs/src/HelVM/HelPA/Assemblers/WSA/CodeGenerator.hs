module HelVM.HelPA.Assemblers.WSA.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
  valueToTL,
  stringToTL
) where

import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.Token
import HelVM.HelPA.Assemblers.WSA.Reducer

import HelVM.HelPA.Common.AssemblyOptions
import HelVM.HelPA.Common.TokenType
import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

reduceAndGenerateCode :: AssemblyOptions -> InstructionList -> String
reduceAndGenerateCode options = generateCode (tokenType options) (startOfInstruction options) (debug options) . reduce (endOfLine options)

generateCode :: TokenType -> Bool -> Bool -> InstructionList -> String
generateCode tokenType startOfInstruction debug = showTLByType tokenType . generateTL startOfInstruction debug

generateTL :: Bool -> Bool -> InstructionList -> TokenList
generateTL startOfInstruction debug  il = generateTLForInstruction startOfInstruction debug  =<< il

generateTLForInstruction :: Bool -> Bool -> Instruction -> TokenList
generateTLForInstruction _    _     EOL = [R]
generateTLForInstruction True debug i   = [E] <> generateTLForInstruction' debug i
generateTLForInstruction _    debug i   = generateTLForInstruction' debug i

generateTLForInstruction' :: Bool -> Instruction -> TokenList
-- Stack instructions
generateTLForInstruction' _ (Push (Literal value)) = [S,S] <> valueToTL value
generateTLForInstruction' _  Dup                   = [S,N,S]
generateTLForInstruction' _  Swap                  = [S,N,T]
generateTLForInstruction' _  Pop                   = [S,N,N]
--Arithmetic
generateTLForInstruction' _ (Add Nothing)          = [T,S,S,S]
generateTLForInstruction' _ (Sub Nothing)          = [T,S,S,T]
generateTLForInstruction' _ (Mul Nothing)          = [T,S,S,N]
generateTLForInstruction' _ (Div Nothing)          = [T,S,T,S]
generateTLForInstruction' _ (Mod Nothing)          = [T,S,T,T]
-- Heap access
generateTLForInstruction' _ (Store Nothing)        = [T,T,S]
generateTLForInstruction' _ (Load  Nothing)        = [T,T,T]
-- Control
generateTLForInstruction' _ (Mark    label)        = [N,S,S] <> stringToTL label
generateTLForInstruction' _ (Call    label)        = [N,S,T] <> stringToTL label
generateTLForInstruction' _ (Branch  label)        = [N,S,N] <> stringToTL label
generateTLForInstruction' _ (BranchZ label)        = [N,T,S] <> stringToTL label
generateTLForInstruction' _ (BranchM label)        = [N,T,T] <> stringToTL label
generateTLForInstruction' _  Return                = [N,T,N]
generateTLForInstruction' _  End                   = [N,N,N]
-- IO instructions
generateTLForInstruction' _  OutputChar            = [T,N,S,S]
generateTLForInstruction' _  OutputNum             = [T,N,S,T]
generateTLForInstruction' _  InputChar             = [T,N,T,S]
generateTLForInstruction' _  InputNum              = [T,N,T,T]
-- Other instructions
generateTLForInstruction' _ Noop                   = []
generateTLForInstruction' True  DebugPrintStack    = [N,N,S,S,S]
generateTLForInstruction' True  DebugPrintHeap     = [N,N,S,S,T]
generateTLForInstruction' False DebugPrintStack    = []
generateTLForInstruction' False DebugPrintHeap     = []
generateTLForInstruction' _ i = error $ "Can not handle instruction " <> show i

valueToTL :: Integer -> TokenList
valueToTL value = integerToTL value <> [N]

integerToTL :: Integer -> TokenList
integerToTL value
  | 0 <= value = S : naturalToTL (fromIntegral value)
  | otherwise  = T : naturalToTL (fromIntegral (- value))

naturalToTL :: Natural -> TokenList
naturalToTL value = bitToToken <$> naturalToDigits2 value

stringToTL :: String -> TokenList
stringToTL value = (charToTL =<< value) <> [N]

charToTL :: Char -> TokenList
charToTL value = bitToToken <$> toBits8 (ord value `mod` 256)

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
