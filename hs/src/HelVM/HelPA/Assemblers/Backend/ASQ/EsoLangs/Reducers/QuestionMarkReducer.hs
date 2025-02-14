module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.QuestionMarkReducer (
  reduceQuestionMarks
) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assembler.Extra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

reduceQuestionMarks :: MonadSafe m => QuestionMark -> ExpressionList -> m ExpressionList
reduceQuestionMarks qm = traverse (reduceQuestionMark qm) . withSymbols

reduceQuestionMark :: MonadSafe m => QuestionMark -> ExpressionWithSymbol -> m Expression
reduceQuestionMark qm (e, currentAddress) = reduceForTE (makeAddress qm currentAddress) e

makeAddress :: QuestionMark -> Symbol -> Symbol
makeAddress CurrentAddress currentAddress = currentAddress
makeAddress NextAddress    currentAddress = currentAddress + 1

reduceForTE :: MonadSafe m => Symbol -> Expression -> m Expression
reduceForTE address (Expression pm t) = makeExpression
  <$> reduceForPmMaybe address pm
  <*> reduceForTerm address t

reduceForPmMaybe :: MonadSafe m => Symbol -> Maybe PMExpression -> m $ Maybe PMExpression
reduceForPmMaybe address = maybe (pure Nothing) (reduceForPm address)

reduceForPm :: MonadSafe m => Symbol -> PMExpression -> m $ Maybe PMExpression
reduceForPm address (PMExpression pm e) = Just . PMExpression pm <$> reduceForTE address e

reduceForTerm :: MonadSafe m => Symbol -> Term -> m Term
reduceForTerm address TermQuestionMark   = pure $ TermSymbol $ Literal address
reduceForTerm address (TermMinus t)      = TermMinus <$> reduceForTerm address t
reduceForTerm address (TermExpression e) = TermExpression <$> reduceForTE address e
reduceForTerm _ t                        = pure t
