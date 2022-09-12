module HelVM.HelPA.Assemblers.ASQ.API.QuestionMark where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

parseQuestionMark :: String -> QuestionMark
parseQuestionMark raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "' is not valid QuestionMark. Valid questionMarks are : " <> show questionMarks

defaultQuestionMark :: QuestionMark
defaultQuestionMark = defaultEnum

questionMarks :: [QuestionMark]
questionMarks = bothEnums

data QuestionMark = CurrentAddress | NextAddress
  deriving stock (Bounded , Enum , Eq , Read , Show)
