module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Files2" $ do
    forM_ [ "true"
          , "hello"
          , "pip"
          , "pip2"
          , "reverse"
          , "function"
          , "add"
          , "writestr"
          , "hello2"
          , "hello3"
          , "hello4"
--          , "writenum"
          , "multiply"
--          , "readnum"
          , "fact"
          , "bottles"
          , "euclid"
          ] $ \file -> do
      it ("test " <> file) $ do
          input <- assemblyFile file
          output <- readEtaFile file
          input `shouldParse` output
