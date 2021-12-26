module HelVM.HelPA.Assemblers.ASQ.Util.CodeGenerator where

import           HelVM.HelPA.Assemblers.ASQ.API.Separator

import           HelVM.HelPA.Assembler.Util

import qualified Data.Text                                as T

generateCode :: Separator -> SymbolList -> Text
generateCode EOL   l = mconcat $ formatSymbol <$> l
generateCode Space l = T.intercalate " " $ show <$> l

formatSymbol :: Symbol -> Text
formatSymbol s = show s <> "\n"