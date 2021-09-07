module HelVM.HelPA.Assemblers.ASQ.AssemblyOptions where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Separator

data AssemblyOptions = AssemblyOptions
  { separator    :: Separator
  , questionMark :: QuestionMark
  }
