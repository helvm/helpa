module HelVM.HelPA.Assemblers.ASQ.API.QuestionMark where

import           HelVM.HelPA.Assembler.API.SwitchEnum

parseQuestionMark :: String -> QuestionMark
parseQuestionMark raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid QuestionMark. Valid questionMarks are : " <> show questionMarks

defaultQuestionMark :: QuestionMark
defaultQuestionMark = defaultEnum

questionMarks :: [QuestionMark]
questionMarks = bothEnums

data QuestionMark = CurrentAddress | NextAddress
  deriving stock (Bounded , Enum , Eq , Read , Show)
