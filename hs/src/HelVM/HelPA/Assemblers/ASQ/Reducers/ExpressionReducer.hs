module HelVM.HelPA.Assemblers.ASQ.Reducers.ExpressionReducer where

import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.HelPA.Assembler.Util
import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Safe

reduceExpressions :: MonadSafeError m => ExpressionList -> m SymbolList
reduceExpressions = traverse reduceForTE

reduceForTE :: MonadSafeError m => Expression -> m Symbol
reduceForTE (Expression  Nothing   t) = reduceForTerm t
reduceForTE (Expression (Just pme) t) = reduceForTE' pme =<< reduceForTerm t where
  reduceForTE' :: MonadSafeError m => PMExpression -> Symbol -> m Symbol
  reduceForTE' (PMExpression pm e) s = execPM pm s <$> reduceForTE e

reduceForTerm :: MonadSafeError m => Term -> m Symbol
reduceForTerm (TermSymbol (Literal s)) = pure s
reduceForTerm (TermExpression      e ) = reduceForTE e
reduceForTerm (TermMinus           t ) = negate <$> reduceForTerm t
reduceForTerm                      t   = liftErrorTuple ("reduceForTerm" , "(" <> show t <> ")")
