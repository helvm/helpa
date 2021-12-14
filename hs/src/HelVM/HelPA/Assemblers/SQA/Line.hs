module HelVM.HelPA.Assemblers.SQA.Line where

import           HelVM.HelPA.Assemblers.SQA.Instruction

-- Constructors
--lineList :: LabelList -> String -> LineList
--lineList ll (c : cs) = lineFromChar ll c : lineListFromStringWithoutLabels cs
--lineList ll [      ] = [Line ll Nothing]
--
--lineListFromStringWithoutLabels :: String -> LineList
--lineListFromStringWithoutLabels s = lineFromCharWithoutLabels <$> s
--
--lineFromLabelList :: LabelList -> Line
--lineFromLabelList ll = Line ll Nothing
--
--lineFromCharWithoutLabels :: Char -> Line
--lineFromCharWithoutLabels = lineFromChar []
--
--lineFromChar :: LabelList -> Char -> Line
--lineFromChar ll  c = lineFromCommand ll $ makeCommandFromChar c

lineFromCommand :: LabelList -> Command -> Line
lineFromCommand ll e = Line ll $ Just e

-- DeConstructors
extractCommands :: [Line] -> [Command]
extractCommands l = extractCommand =<< l

extractCommand :: Line -> [Command]
extractCommand = maybeToList . maybeCommand

-- Types
type LineList = [Line]

data Line = Line { labelList :: !LabelList , maybeCommand :: Maybe Command }
  deriving stock (Eq, Show, Ord)
