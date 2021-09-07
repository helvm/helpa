module HelVM.HelPA.Assemblers.ASQ.API.QuestionMark where

data QuestionMark = CurrentAddress | NextAddress
  deriving stock (Eq , Read , Show)

questionMarks :: [QuestionMark]
questionMarks = [CurrentAddress , NextAddress]

defaultQuestionMark :: QuestionMark
defaultQuestionMark = CurrentAddress

parseQuestionMark :: String -> QuestionMark
parseQuestionMark raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid QuestionMark. Valid questionMarks are : " <> show questionMarks
