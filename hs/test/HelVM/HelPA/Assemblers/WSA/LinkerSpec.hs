module HelVM.HelPA.Assemblers.WSA.LinkerSpec where

import HelVM.HelPA.Assemblers.WSA.Linker
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil
import HelVM.HelPA.Assemblers.Util

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Files" $ do

    it "io"     $ do linkLibFile "io"     `shouldReturn` ioIL
    it "memory" $ do linkLibFile "memory" `shouldReturn` memoryIL
    it "prim"   $ do linkFile    "prim"   `shouldReturn` (primIL <> ioIL)
