module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.ExpressionReducer where

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assembler.Extra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe

reduceExpressions :: MonadSafe m => ExpressionList -> m SymbolList
reduceExpressions = traverse reduceForTE

reduceForTE :: MonadSafe m => Expression -> m Symbol
reduceForTE (Expression  Nothing   t) = reduceForTerm t
reduceForTE (Expression (Just pme) t) = reduceForTE' pme =<< reduceForTerm t where
  reduceForTE' :: MonadSafe m => PMExpression -> Symbol -> m Symbol
  reduceForTE' (PMExpression pm e) s = execPM pm s <$> reduceForTE e

reduceForTerm :: MonadSafe m => Term -> m Symbol
reduceForTerm (TermSymbol (Literal s)) = pure s
reduceForTerm (TermExpression      e ) = reduceForTE e
reduceForTerm (TermMinus           t ) = negate <$> reduceForTerm t
reduceForTerm                      t   = liftErrorTuple ("reduceForTerm" , "(" <> show t <> ")")
