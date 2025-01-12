module HelVM.HelPA.Assemblers.Frontend.HPL.Instruction where

data Element
  = Asm { inline :: Bool, name :: Text, args :: [Text], body :: Text }
  | Def { inline :: Bool, name :: Text, args :: [Text], body :: Text }
  | Section { title :: Text }
  deriving stock (Show)
