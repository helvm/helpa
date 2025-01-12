module HelVM.HelPA.Assembler.AsmParser.Extra where

import           HelVM.HelIO.ReadText

import           Data.Char

isNewline :: Char -> Bool
isNewline c = c == '\n'

isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || '_' == c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || '_' == c

isAlphaNumDot_ :: Char -> Bool
isAlphaNumDot_ c = isAlphaNum_ c || '.' == c

isPlusMinus :: Char -> Bool
isPlusMinus c = '+' == c || '-' == c

unEscape :: String -> String
unEscape s = readUnsafe $ "\"" <> s <> "\""
