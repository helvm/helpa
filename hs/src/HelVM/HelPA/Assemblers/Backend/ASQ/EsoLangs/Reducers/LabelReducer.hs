module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Reducers.LabelReducer (
  reduceLabels
) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Instruction
import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Line

import           HelVM.HelPA.Assembler.Extra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Containers.Extra

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import qualified Data.Map                                                as Map

reduceLabels :: MonadSafe m => Bool -> LineList -> m ExpressionList
reduceLabels addOutLabel l = reduceForTEList (addressOfLabels addOutLabel l) $ extractExpressions l

addressOfLabels :: Bool -> LineList -> LabelSymbols
addressOfLabels addOutLabel l = addOutLabelSymbol addOutLabel $ toMap $ withSymbols $ labelList <$> l

addOutLabelSymbol :: Bool -> LabelSymbols -> LabelSymbols
addOutLabelSymbol True = Map.insert "OUT" (-1)
addOutLabelSymbol _    = id

reduceForTEList :: MonadSafe m => LabelSymbols -> ExpressionList -> m ExpressionList
reduceForTEList addresses = traverse (reduceForTE addresses)

reduceForTE :: MonadSafe m => LabelSymbols -> Expression -> m Expression
reduceForTE addresses (Expression pm t) = makeExpression
  <$> reduceForPmMaybe addresses pm
  <*> reduceForTerm addresses t

reduceForPmMaybe :: MonadSafe m => LabelSymbols -> Maybe PMExpression -> m $ Maybe PMExpression
reduceForPmMaybe addresses = maybe (pure Nothing) (reduceForPm addresses)

reduceForPm :: MonadSafe f => LabelSymbols -> PMExpression -> f (Maybe PMExpression)
reduceForPm addresses (PMExpression pm e) = Just . PMExpression pm <$> reduceForTE addresses e

reduceForTerm :: MonadSafe m => LabelSymbols -> Term -> m Term
reduceForTerm addresses (TermSymbol (Variable identifier)) = TermSymbol . Literal <$> indexSafeByKey identifier addresses
reduceForTerm addresses (TermMinus t)                      = TermMinus <$> reduceForTerm addresses t
reduceForTerm addresses (TermExpression e)                 = TermExpression <$> reduceForTE addresses e
reduceForTerm _ t                                          = pure t
