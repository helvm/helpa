module HelVM.HelPA.Assemblers.Frontend.WSA.CodeGenerator (
  reduceAndGenerateCode,
) where

import           HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptions
import           HelVM.HelPA.Assemblers.Backend.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction
import           HelVM.HelPA.Assemblers.Frontend.WSA.Reducer        hiding (addEndOfLine, addEndOfLineMaybe)

import           HelVM.HelIO.Control.Safe

reduceAndGenerateCode :: MonadSafe m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (tokenType options) (startOfInstruction options) (debug options) $ reduce (endOfLine options) il
