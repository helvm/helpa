module HelVM.HelPA.Assemblers.ASQ.AssemblyOptionsExtra where

import           HelVM.HelPA.Assemblers.ASQ.API.AssemblyOptions
import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark
import           HelVM.HelPA.Assemblers.ASQ.API.Version

import           HelVM.HelPA.Assembler.API.Separator

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

