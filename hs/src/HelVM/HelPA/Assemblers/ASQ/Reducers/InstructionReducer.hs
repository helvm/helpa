module HelVM.HelPA.Assemblers.ASQ.Reducers.InstructionReducer (
  reduceIL
) where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.Common.Safe

reduceIL :: MonadSafeError m => QuestionMark -> InstructionList -> m ItemList
reduceIL qm il = join <$> traverse (reduceInstruction qm) il

reduceInstruction :: MonadSafeError m => QuestionMark -> Instruction -> m ItemList
reduceInstruction _  (Instruction Data a) = pure a
reduceInstruction qm (Instruction Code a) = buildItems qm a

buildItems :: MonadSafeError m => QuestionMark -> ItemList -> m ItemList
buildItems qm its = checkItems $ filter isExpression its where
  checkItems [it] = pure $ buildSingle qm it $ filter nonExpression its
  checkItems its' = checkLength $ length its' where
    checkLength 2 = pure $ its <> [endItem qm]
    checkLength 3 = pure its
    checkLength 0 = pure its
    checkLength _ = liftError $ show its

buildSingle :: QuestionMark -> Item -> ItemList -> ItemList
buildSingle mq it its = its <> [it , changeItem it , endItem mq]

endItem :: QuestionMark -> Item
endItem CurrentAddress = ItemExpression makeNextAddress
endItem NextAddress    = ItemExpression makeCurrentAddress

changeItem :: Item -> Item
changeItem (ItemExpression e) = ItemExpression $ changeExpression e
changeItem                 i  = i

changeExpression :: Expression -> Expression
changeExpression (Expression  Nothing  t) = makeExpressionWithoutPM $ changeTerm t
changeExpression (Expression (Just pm) t) = makeExpressionWithPM (changePMExpression pm) $ changeTerm t

changeTerm :: Term -> Term
changeTerm TermQuestionMark   = TermExpression makePrevAddress
changeTerm (TermExpression e) = TermExpression $ changeExpression e
changeTerm t                  = t

changePMExpression :: PMExpression -> PMExpression
changePMExpression (PMExpression pm e) = PMExpression pm $ changeExpression e
