module HelVM.HelPA.Assemblers.ASQ.Reducers.QuestionMarkReducer (
  reduceQuestionMarks
) where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.HelPA.Assembler.Util
import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Safe

import           Control.Type.Operator

reduceQuestionMarks :: MonadSafeError m => QuestionMark -> ExpressionList -> m ExpressionList
reduceQuestionMarks qm l = traverse (reduceQuestionMark qm) $ withSymbols l

reduceQuestionMark :: MonadSafeError m => QuestionMark -> ExpressionWithSymbol -> m Expression
reduceQuestionMark qm (e, currentAddress) = reduceForTE (makeAddress qm currentAddress) e

makeAddress :: QuestionMark -> Symbol -> Symbol
makeAddress CurrentAddress currentAddress = currentAddress
makeAddress NextAddress    currentAddress = currentAddress + 1

reduceForTE :: MonadSafeError m => Symbol -> Expression -> m Expression
reduceForTE address (Expression pm t) = liftA2 makeExpression pm' t' where
  pm' = reduceForPmMaybe address pm
  t'  = reduceForTerm    address t

reduceForPmMaybe :: MonadSafeError m => Symbol -> Maybe PMExpression -> m $ Maybe PMExpression
reduceForPmMaybe _        Nothing                   = pure Nothing
reduceForPmMaybe address (Just (PMExpression pm e)) = Just . PMExpression pm <$> reduceForTE address e

reduceForTerm :: MonadSafeError m => Symbol -> Term -> m Term
reduceForTerm address TermQuestionMark   = pure $ TermSymbol $ Literal address
reduceForTerm address (TermMinus t)      = TermMinus <$> reduceForTerm address t
reduceForTerm address (TermExpression e) = TermExpression <$> reduceForTE address e
reduceForTerm _ t                        = pure t
