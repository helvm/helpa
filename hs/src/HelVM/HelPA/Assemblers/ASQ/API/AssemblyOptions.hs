module HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions where

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Separator
import           HelVM.HelPA.Assemblers.ASQ.API.Version

data AssemblyOptions = AssemblyOptions
  { separator    :: Separator
  , questionMark :: QuestionMark
  , addOutLabel  :: Bool
  , version      :: Version
  }
