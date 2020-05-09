{-# Language NamedFieldPuns   #-}

module Main where

import HelVM.HelPA.Common.Util

import qualified HelVM.HelPA.Assemblers.EAS.Assembler as EAS
import qualified HelVM.HelPA.Assemblers.WSA.Assembler as WSA

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
run AppOptions{lang, debug, dir, file} = do
  eval (computeLang lang) debug dir file
  
eval :: Lang -> Bool -> String -> String -> IO ()
eval EAS    _     dir file = putExcept $ EAS.assemblyIO dir file
eval WSA    debug dir file = putExcept $ WSA.assemblyIO debug dir file
eval HAPAPL _     dir file = hapapl dir file

putExcept :: IO (Either String String) -> IO ()
putExcept io = putStrLn . output =<< io

output :: Either String String -> String
output (Right result) = result
output (Left message) = error message

hapapl :: String -> String -> IO ()
hapapl _ _ = putStrLn "HAPAPL is not supported now"
