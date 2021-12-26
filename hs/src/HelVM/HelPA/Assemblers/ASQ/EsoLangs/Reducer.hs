module HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducer where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.ExpressionReducer
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.InstructionReducer
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.ItemReducer
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.LabelReducer
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.QuestionMarkReducer

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assembler.Util

import           HelVM.Common.Safe

reduce :: MonadSafe m => Bool -> QuestionMark -> InstructionList -> m SymbolList
reduce addOutLabel qm il = reduceExpressions =<< (reduceQuestionMarks qm =<< (reduceLabels addOutLabel . reduceItemList =<< reduceIL qm il))
