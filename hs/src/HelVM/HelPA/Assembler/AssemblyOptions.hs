module HelVM.HelPA.Assembler.AssemblyOptions where

import HelVM.HelPA.Assembler.TokenType

manyOptionsWithName :: [(String , AssemblyOptions)]
manyOptionsWithName = [
             ( "bothTokenTypeOptions"    , bothTokenTypeOptions )
           , ( "visibleTokenTypeOptions" , visibleTokenTypeOptions )
           , ( "debugOptions"            , debugOptions   )
           , ( "soiOptions"              , soiOptions     )
           , ( "eolOptions"              , eolOptions     )
           , ( "dsoiOptions"             , dsoiOptions    )
           , ( "deolOptions"             , deolOptions    )
           , ( "allTrue"                 , allTrue        )
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

data AssemblyOptions = AssemblyOptions
  { tokenType          :: !TokenType
  , debug              :: !Bool
  , startOfInstruction :: !Bool
  , endOfLine          :: !Bool
  }
