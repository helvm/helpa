module HelVM.HelPA.Assemblers.SQA.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.SQA.AsmParser
import           HelVM.HelPA.Assemblers.SQA.FileUtil
--import           HelVM.HelPA.Assemblers.SQA.Instruction

--import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Safe

--import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec = do

  describe "parseAssemblyFile" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
    forM_ [ "eigenratios" </> "origin"
          ] $ \fileName -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToSqaFile fileName
      it fileName $ do safeIOToPTextIO parseAssembly `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyText" </> fileName)

--  describe "parseAssemblyText" $ do
--    forM_ [ ("subleq A1   A1\n"  , [Instruction Code [ ItemExpression ItemExpression (Expression Nothing (TermSymbol (Variable "subleq"))),Expression Nothing (TermSymbol (Variable "A1"))),ItemExpression (Expression Nothing (TermSymbol (Variable "A1")))]])
----          , ("s: subleq A1 A1\n" , [Instruction Code [ ItemExpression (Expression Nothing (TermSymbol (Variable "A1"))),ItemExpression (Expression Nothing (TermSymbol (Variable "A1")))]])
--          ] $ \(line , il) -> do
--      it line $ do parseAssemblyText (toText line) `shouldSafe` il
