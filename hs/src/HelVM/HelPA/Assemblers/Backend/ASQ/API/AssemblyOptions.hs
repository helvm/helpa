module HelVM.HelPA.Assemblers.Backend.ASQ.API.AssemblyOptions where

import           HelVM.HelPA.Assemblers.Backend.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.Backend.ASQ.API.Version

import           HelVM.HelPA.Assembler.API.Separator

data AssemblyOptions = AssemblyOptions
  { separator    :: Separator
  , questionMark :: QuestionMark
  , addOutLabel  :: Bool
  , version      :: Version
  }
