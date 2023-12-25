module HelVM.HelPA.Assemblers.Frontend.SblAsm.AsmParser (parseAssemblyText) where

import           HelVM.HelPA.Assemblers.Frontend.SblAsm.Instruction

import           HelVM.HelIO.Control.Safe

parseAssemblyText :: MonadSafe m => Text -> m InstructionList
parseAssemblyText _ = pure []
