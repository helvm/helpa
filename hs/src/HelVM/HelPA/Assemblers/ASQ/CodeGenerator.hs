module HelVM.HelPA.Assemblers.ASQ.CodeGenerator (
  reduceAndGenerateCode,
  generateCode,
) where

import           HelVM.HelPA.Assemblers.ASQ.API.Separator

import           HelVM.HelPA.Assemblers.ASQ.AssemblyOptions
import           HelVM.HelPA.Assemblers.ASQ.Instruction
import           HelVM.HelPA.Assemblers.ASQ.Reducer

import           HelVM.HelPA.Assembler.Util

import           HelVM.Common.Safe

import qualified Data.Text                                  as T

reduceAndGenerateCode :: MonadSafeError m => AssemblyOptions -> InstructionList -> m Text
reduceAndGenerateCode options il = generateCode (separator options) <$> reduce (questionMark options) il

generateCode :: Separator -> SymbolList -> Text
generateCode EOL   l = mconcat $ formatSymbol <$> l
generateCode Space l = T.intercalate " " $ show <$> l

formatSymbol :: Symbol -> Text
formatSymbol s = show s <> "\n"
