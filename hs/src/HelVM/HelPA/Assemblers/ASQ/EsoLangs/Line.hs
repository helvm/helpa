module HelVM.HelPA.Assemblers.ASQ.EsoLangs.Line where

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction

-- Constructors
lineList :: LabelList -> String -> LineList
lineList ll (c : cs) = lineFromChar ll c : lineListFromStringWithoutLabels cs
lineList ll [      ] = [Line ll Nothing]

lineListFromStringWithoutLabels :: String -> LineList
lineListFromStringWithoutLabels s = lineFromCharWithoutLabels <$> s

lineFromLabelList :: LabelList -> Line
lineFromLabelList ll = Line ll Nothing

lineFromCharWithoutLabels :: Char -> Line
lineFromCharWithoutLabels = lineFromChar []

lineFromChar :: LabelList -> Char -> Line
lineFromChar ll  c = lineFromExpression ll $ makeExpressionFromChar c

lineFromExpression :: LabelList -> Expression -> Line
lineFromExpression ll e = Line ll $ Just e

-- DeConstructors
extractExpressions :: [Line] -> [Expression]
extractExpressions l = extractExpression =<< l

extractExpression :: Line -> [Expression]
extractExpression = maybeToList . maybeExpression

-- Types
type LineList = [Line]

data Line = Line { labelList :: !LabelList , maybeExpression :: Maybe Expression }
  deriving stock (Eq, Show, Ord)
