module HelVM.HelPA.Assemblers.WSA.AssemblerSpec where

import HelVM.HelPA.Assemblers.WSA.Assembler
import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.CodeGenerator

import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil
import HelVM.HelPA.Assemblers.Util

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Examples" $ do
    it "io"     $ do assembleLibFile "io"     `shouldReturn` showTL ioTL
    it "memory" $ do assembleLibFile "memory" `shouldReturn` showTL memoryTL
    it "prim"   $ do assembleFile    "prim"   `shouldReturn` showTL primTL
