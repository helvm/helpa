module HelVM.HelPA.Assemblers.ASQ.API.QuestionMark where

import           HelVM.HelIO.SwitchEnum

defaultQuestionMark :: QuestionMark
defaultQuestionMark = defaultEnum

questionMarks :: [QuestionMark]
questionMarks = bothEnums

data QuestionMark = CurrentAddress | NextAddress
  deriving stock (Bounded , Enum , Eq , Read , Show)
