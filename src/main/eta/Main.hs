{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelPA.Common.Util

import qualified HelVM.HelPA.Assemblers.EAS.Assembler as EAS

import AppOptions

import Options.Applicative
import System.IO

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelPA: Heavenly Esoteric Little Para Assembler to Esoteric Languages implemented in Haskell/Eta"
     <> progDesc "" )

run :: AppOptions -> IO ()
run AppOptions{lang, dir, file} = do
  eval (computeLang lang) dir file
  
eval :: Lang -> String -> String -> IO ()
eval EAS    dir file = putExcept $ EAS.assemblyIO dir file
eval HAPAPL dir file = hapapl dir file

putExcept :: IO (Either String String) -> IO ()
putExcept io = putStrLn =<< (output <$> io)

output :: Either String String -> String
output (Right result) = result
output (Left message) = error message

hapapl :: String -> String -> IO ()
hapapl _ _ = putStrLn "HAPAPL is not supported now"
