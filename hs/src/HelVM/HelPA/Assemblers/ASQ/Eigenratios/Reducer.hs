module HelVM.HelPA.Assemblers.ASQ.Eigenratios.Reducer where

import           HelVM.HelPA.Assemblers.ASQ.Eigenratios.Instruction

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import qualified HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction    as ASQ
import qualified HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducer        as ASQ

import           HelVM.HelPA.Assembler.Util

import           HelVM.Common.Control.Safe

reduce :: MonadSafe m => Bool -> QuestionMark -> InstructionList -> m SymbolList
reduce addOutLabel qm il = ASQ.reduce addOutLabel qm $ reduceIL il

reduceIL :: InstructionList -> ASQ.InstructionList
reduceIL il = reduceInstruction =<< il

reduceInstruction :: Instruction -> ASQ.InstructionList
reduceInstruction (Instruction l c) = catMaybes [reduceLabel <$> l , reduceCommand <$> c]

reduceLabel :: Label -> ASQ.Instruction
reduceLabel l = ASQ.makeDataInstruction [ASQ.ItemLabel l]

reduceCommand :: Command -> ASQ.Instruction
reduceCommand (Data v              ) = ASQ.makeDataInstructionFromIntegerValue v
reduceCommand (Code v1 v2  Nothing ) = ASQ.makeCodeInstructionFromIntegerValueList [v1 , v2]
reduceCommand (Code v1 v2 (Just v3)) = ASQ.makeCodeInstructionFromIntegerValueList [v1 , v2 , v3]
