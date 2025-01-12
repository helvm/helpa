module HelVM.HelPA.Assembler.AsmParser.Mega.Extra where

import           HelVM.HelPA.Assembler.AsmParser.Mega.Parsers

import           Text.Megaparsec                              hiding (Label, many)

parseLineWith :: Parser b -> String -> Parser b
parseLineWith p s = safeParse p s =<< readTillNewline

safeParse :: MonadFail m => Parser a -> String -> Text -> m a
safeParse p s = either (fail . show) pure . runParser p s

readTillNewline :: Parser Text
readTillNewline = toText <$> takeWhileP Nothing (/= '\n')
