module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducer where

import           HelVM.HelPA.Assemblers.Backend.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.ExpressionReducer
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.InstructionReducer
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.ItemReducer
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.LabelReducer
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.QuestionMarkReducer

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assembler.Extra

import           HelVM.HelIO.Control.Safe

reduce :: MonadSafe m => Bool -> QuestionMark -> InstructionList -> m SymbolList
reduce addOutLabel qm il = reduceExpressions =<< (reduceQuestionMarks qm =<< (reduceLabels addOutLabel . reduceItemList =<< reduceIL qm il))
