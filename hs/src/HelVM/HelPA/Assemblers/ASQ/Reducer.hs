module HelVM.HelPA.Assemblers.ASQ.Reducer where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.ASQ.Reducers.ExpressionReducer
import           HelVM.HelPA.Assemblers.ASQ.Reducers.InstructionReducer
import           HelVM.HelPA.Assemblers.ASQ.Reducers.ItemReducer
import           HelVM.HelPA.Assemblers.ASQ.Reducers.LabelReducer
import           HelVM.HelPA.Assemblers.ASQ.Reducers.QuestionMarkReducer

import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.HelPA.Assembler.Util

import           HelVM.Common.Safe

reduce :: MonadSafeError m => QuestionMark -> InstructionList -> m SymbolList
reduce qm il = reduceExpressions =<< (reduceQuestionMarks qm =<< (reduceLabels . reduceItemList =<< reduceIL qm il))
