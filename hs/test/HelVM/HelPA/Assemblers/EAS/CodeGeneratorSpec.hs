module HelVM.HelPA.Assemblers.EAS.CodeGeneratorSpec where

import HelVM.HelPA.Assemblers.EAS.CodeGenerator

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil

import Test.Hspec

spec :: Spec
spec = do
  describe "showValue" $ do
    it "showValue 0" $ do showValue 0 `shouldBe` ""
    it "showValue 1" $ do showValue 1 `shouldBe` "t"
    it "showValue 10" $ do showValue 10 `shouldBe` "to"
    it "showValue 11" $ do showValue 11 `shouldBe` "ti"
    it "showValue 110" $ do showValue 110 `shouldBe` "atn"
    it "showValue 111" $ do showValue 111 `shouldBe` "ats"
    it "showValue 1110" $ do showValue 1110 `shouldBe` "otii"
    it "showValue 1111" $ do showValue 1111 `shouldBe` "otin"
    it "showValue 11110" $ do showValue 11110 `shouldBe` "iiant"
    it "showValue 11111" $ do showValue 11111 `shouldBe` "iiana"
    it "showValue 111110" $ do showValue 111110 `shouldBe` "sitsos"
    it "showValue 111111" $ do showValue 111111 `shouldBe` "sitsih"

  describe "Examples" $ do
    it "true"     $ do generateCode trueIL            `shouldBeDo` readEtaFile "true"
    it "hello"    $ do generateCode helloIL           `shouldBeDo` readEtaFile "hello"
    it "pip"      $ do generateCode pipILReduced      `shouldBeDo` readEtaFile "pip"
    it "pip2"     $ do generateCode pip2ILReduced     `shouldBeDo` readEtaFile "pip2"
    it "reverse"  $ do generateCode reverseILReduced  `shouldBeDo` readEtaFile "reverse"
    it "function" $ do generateCode functionIL        `shouldBeDo` readEtaFile "function"
    it "add"      $ do generateCode addILReduced      `shouldBeDo` readEtaFile "add"
    it "writestr" $ do generateCode writeStrILReduced `shouldBeDo` readEtaFile "writestr"
    it "hello2"   $ do generateCode hello2ILReduced   `shouldBeDo` readEtaFile "hello2"
    it "writenum" $ do generateCode writeNumILReduced `shouldBeDo` readEtaFile "writenum"
    it "multiply" $ do generateCode multiplyILReduced `shouldBeDo` readEtaFile "multiply"
    it "readnum"  $ do generateCode readNumILReduced  `shouldBeDo` readEtaFile "readnum"
    it "fact"     $ do generateCode factILReduced     `shouldBeDo` readEtaFile "fact"
    it "bottles"  $ do generateCode bottlesILReduced  `shouldBeDo` readEtaFile "bottles"
    it "euclid"   $ do generateCode euclidILReduced   `shouldBeDo` readEtaFile "euclid"

shouldBeDo :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
action `shouldBeDo` expected = (action `shouldBe`) =<< expected