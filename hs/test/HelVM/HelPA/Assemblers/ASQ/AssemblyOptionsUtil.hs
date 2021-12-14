module HelVM.HelPA.Assemblers.ASQ.AssemblyOptionsUtil where

import           HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions

import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Separator
import           HelVM.HelPA.Assemblers.ASQ.API.Version

defaultAssemblyOptionsForTest:: AssemblyOptions
defaultAssemblyOptionsForTest = defaultAssemblyOptions {separator = EOL}

defaultAssemblyOptionsWithNextAddress :: AssemblyOptions
defaultAssemblyOptionsWithNextAddress = defaultAssemblyOptions {questionMark = NextAddress}

defaultAssemblyOptions :: AssemblyOptions
defaultAssemblyOptions = AssemblyOptions {
    addOutLabel  = False
  , separator    = defaultSeparator
  , questionMark = defaultQuestionMark
  , version      = defaultVersion
  }

