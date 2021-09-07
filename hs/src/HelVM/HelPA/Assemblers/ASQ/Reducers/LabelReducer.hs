module HelVM.HelPA.Assemblers.ASQ.Reducers.LabelReducer (
  reduceLabels
) where

import           HelVM.HelPA.Assemblers.ASQ.Instruction
import           HelVM.HelPA.Assemblers.ASQ.Line

import           HelVM.HelPA.Assembler.Util
import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Containers.Util

import           HelVM.Common.Safe

import           Control.Type.Operator

import qualified Data.Map                               as Map

reduceLabels :: MonadSafeError m => LineList -> m ExpressionList
reduceLabels l = reduceForTEList (addressOfLabels l) $ extractExpressions l

addressOfLabels :: LineList -> LabelSymbols
addressOfLabels l = Map.insert "OUT" (-1) (toMap $ withSymbols $ labelList <$> l)

reduceForTEList :: MonadSafeError m => LabelSymbols -> ExpressionList -> m ExpressionList
reduceForTEList addresses = traverse (reduceForTE addresses)

reduceForTE :: MonadSafeError m => LabelSymbols -> Expression -> m Expression
reduceForTE addresses (Expression pm t) = liftA2 makeExpression pm' t' where
  pm' = reduceForPmMaybe addresses pm
  t'  = reduceForTerm    addresses t

reduceForPmMaybe :: MonadSafeError m => LabelSymbols -> Maybe PMExpression -> m $ Maybe PMExpression
reduceForPmMaybe _          Nothing                   = pure Nothing
reduceForPmMaybe addresses (Just (PMExpression pm e)) = Just . PMExpression pm <$> reduceForTE addresses e

reduceForTerm :: MonadSafeError m => LabelSymbols -> Term -> m Term
reduceForTerm addresses (TermSymbol (Variable identifier)) = TermSymbol . Literal <$> indexSafeByKey identifier addresses
reduceForTerm addresses (TermMinus t)                      = TermMinus <$> reduceForTerm addresses t
reduceForTerm addresses (TermExpression e)                 = TermExpression <$> reduceForTE addresses e
reduceForTerm _ t                                          = pure t
