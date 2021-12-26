module HelVM.HelPA.Assemblers.WSA.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
  valueToTL,
  identifierToTL
) where

import           HelVM.HelPA.Assemblers.WSA.AssemblyOptions
import           HelVM.HelPA.Assemblers.WSA.Instruction
import           HelVM.HelPA.Assemblers.WSA.Reducer
import           HelVM.HelPA.Assemblers.WSA.Token

import           HelVM.HelPA.Assembler.Value
import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import           HelVM.Common.Digit.Digitable
import           HelVM.Common.Safe

reduceAndGenerateCode :: MonadSafe m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (tokenType options) (startOfInstruction options) (debug options) $ reduce (endOfLine options) il

generateCode :: MonadSafe m => TokenType -> Bool -> Bool -> InstructionList -> m Text
generateCode tokenType startOfInstruction debug il = showTLByType tokenType <$> generateTL startOfInstruction debug il

generateTL :: MonadSafe m => Bool -> Bool -> InstructionList -> m TokenList
generateTL startOfInstruction debug il = join <$> traverse (generateTLForInstruction startOfInstruction debug) il

generateTLForInstruction :: MonadSafe m => Bool -> Bool -> Instruction -> m TokenList
generateTLForInstruction _    _     EOL = pure [R]
generateTLForInstruction True debug i   = ([E] <>) <$> generateTLForInstruction' debug i
generateTLForInstruction _    debug i   = generateTLForInstruction' debug i

generateTLForInstruction' :: MonadSafe m => Bool -> Instruction -> m TokenList
-- | Stack instructions
generateTLForInstruction' _ (Push (Literal value)) = ([S,S] <>) <$> valueToTL value
generateTLForInstruction' _  Dup                   = pure [S,N,S]
generateTLForInstruction' _  Swap                  = pure [S,N,T]
generateTLForInstruction' _  Pop                   = pure [S,N,N]
-- | Arithmetic
generateTLForInstruction' _ (Add Nothing)          = pure [T,S,S,S]
generateTLForInstruction' _ (Sub Nothing)          = pure [T,S,S,T]
generateTLForInstruction' _ (Mul Nothing)          = pure [T,S,S,N]
generateTLForInstruction' _ (Div Nothing)          = pure [T,S,T,S]
generateTLForInstruction' _ (Mod Nothing)          = pure [T,S,T,T]
-- | Heap access
generateTLForInstruction' _ (Store Nothing)        = pure [T,T,S]
generateTLForInstruction' _ (Load  Nothing)        = pure [T,T,T]
-- | Control
generateTLForInstruction' _ (Mark    label)        = ([N,S,S] <>) <$> identifierToTL label
generateTLForInstruction' _ (Call    label)        = ([N,S,T] <> ) <$> identifierToTL label
generateTLForInstruction' _ (Branch  label)        = ([N,S,N] <> ) <$> identifierToTL label
generateTLForInstruction' _ (BranchZ label)        = ([N,T,S] <> ) <$> identifierToTL label
generateTLForInstruction' _ (BranchM label)        = ([N,T,T] <> ) <$>identifierToTL label
generateTLForInstruction' _  Return                = pure [N,T,N]
generateTLForInstruction' _  End                   = pure [N,N,N]
-- | IO instructions
generateTLForInstruction' _  OutputChar            = pure [T,N,S,S]
generateTLForInstruction' _  OutputNum             = pure [T,N,S,T]
generateTLForInstruction' _  InputChar             = pure [T,N,T,S]
generateTLForInstruction' _  InputNum              = pure [T,N,T,T]
-- | Other instructions
generateTLForInstruction' _ Noop                   = pure []
generateTLForInstruction' True  DebugPrintStack    = pure [N,N,S,S,S]
generateTLForInstruction' True  DebugPrintHeap     = pure [N,N,S,S,T]
generateTLForInstruction' False DebugPrintStack    = pure []
generateTLForInstruction' False DebugPrintHeap     = pure []
generateTLForInstruction' _ i                      = liftErrorTuple ("Can not handle instruction" , show i)

valueToTL :: MonadSafe m => Integer -> m TokenList
valueToTL value = integerToTL value <&> (<> [N])

integerToTL :: MonadSafe m => Integer -> m TokenList
integerToTL value
  | 0 <= value = (S : ) <$> naturalToDL (fromIntegral value)
  | otherwise  = (T : ) <$> naturalToDL (fromIntegral (- value))

identifierToTL :: MonadSafe m => Identifier -> m TokenList
identifierToTL i = stringToDL (unwrapIdentifier i) <&> (<> [N])
