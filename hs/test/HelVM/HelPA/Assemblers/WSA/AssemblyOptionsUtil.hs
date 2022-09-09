module HelVM.HelPA.Assemblers.WSA.AssemblyOptionsUtil where

import           HelVM.HelPA.Assemblers.WSA.AssemblyOptions

import           HelVM.HelPA.Assemblers.WSA.API.TokenType

import           HelVM.HelIO.NamedValue

manyOptionsWithName :: [NamedValue AssemblyOptions]
manyOptionsWithName =
           [ NamedValue "bothTokenTypeOptions"    bothTokenTypeOptions
           , NamedValue "visibleTokenTypeOptions" visibleTokenTypeOptions
           , NamedValue "debugOptions"            debugOptions
           , NamedValue "soiOptions"              soiOptions
           , NamedValue "eolOptions"              eolOptions
           , NamedValue "dsoiOptions"             dsoiOptions
           , NamedValue "deolOptions"             deolOptions
           , NamedValue "allTrue"                 allTrue
           ]

bothTokenTypeOptions :: AssemblyOptions
bothTokenTypeOptions = AssemblyOptions {tokenType=BothTokenType , debug=False , startOfInstruction=False , endOfLine=False}

visibleTokenTypeOptions :: AssemblyOptions
visibleTokenTypeOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=False , startOfInstruction=False , endOfLine=False}

debugOptions :: AssemblyOptions
debugOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=True , startOfInstruction=False , endOfLine=False}

soiOptions :: AssemblyOptions
soiOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=False , startOfInstruction=True , endOfLine=False}

eolOptions :: AssemblyOptions
eolOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=False , startOfInstruction=False , endOfLine=True}

dsoiOptions :: AssemblyOptions
dsoiOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=True , startOfInstruction=True , endOfLine=False}

deolOptions :: AssemblyOptions
deolOptions = AssemblyOptions {tokenType=VisibleTokenType , debug=True , startOfInstruction=False , endOfLine=True}

allTrue :: AssemblyOptions
allTrue = AssemblyOptions {tokenType=VisibleTokenType , debug=True , startOfInstruction=True , endOfLine=True}

allFalse :: AssemblyOptions
allFalse = AssemblyOptions {tokenType=WhiteTokenType , debug=False , startOfInstruction=False , endOfLine=False}
