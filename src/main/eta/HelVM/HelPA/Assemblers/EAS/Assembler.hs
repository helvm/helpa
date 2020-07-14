module HelVM.HelPA.Assemblers.EAS.Assembler where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.Translator

import qualified Data.Text as T

assembly :: T.Text -> Either String String
assembly t = fmap (codeGeneration . translate) $ parseAssembler t
--assembly t = Right $ T.unpack t

assembly2 :: T.Text -> Either String String
assembly2 t = do
  il <- parseAssembler t
  let addresses = addressOfLabels il
  let il' = replaceLabels addresses il
  return $ codeGeneration il'
