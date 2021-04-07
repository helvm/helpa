module HelVM.HelPA.Assemblers.EAS.LinkerSpec where

import HelVM.HelPA.Assemblers.EAS.Linker
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil
import HelVM.HelPA.Assemblers.Util

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Files" $ do

    it "true"     $ do linkFile "true"     `shouldReturn` trueIL
    it "hello"    $ do linkFile "hello"    `shouldReturn` helloIL
    it "pip"      $ do linkFile "pip"      `shouldReturn` pipIL
    it "pip2"     $ do linkFile "pip2"     `shouldReturn` pip2IL
    it "reverse"  $ do linkFile "reverse"  `shouldReturn` reverseIL
    it "function" $ do linkFile "function" `shouldReturn` functionIL
    it "writestr" $ do linkFile "writestr" `shouldReturn` writeStrIL
    it "hello2"   $ do linkFile "hello2"   `shouldReturn` hello2ILLinked
    it "hello3"   $ do linkFile "hello3"   `shouldReturn` hello2ILLinked
    it "hello4"   $ do linkFile "hello4"   `shouldReturn` hello4ILLinked
    it "writenum" $ do linkFile "writenum" `shouldReturn` writeNumIL
    it "multiply" $ do linkFile "multiply" `shouldReturn` multiplyIL
    it "readnum"  $ do linkFile "readnum"  `shouldReturn` readNumIL
    it "fact"     $ do linkFile "fact"     `shouldReturn` factILLinked
    it "bottles"  $ do linkFile "bottles"  `shouldReturn` bottlesILLinked
    it "euclid"   $ do linkFile "euclid"   `shouldReturn` euclidIL

