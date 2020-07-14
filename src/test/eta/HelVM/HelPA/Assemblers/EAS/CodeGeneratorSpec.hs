module HelVM.HelPA.Assemblers.EAS.CodeGeneratorSpec where

import HelVM.HelPA.Assemblers.EAS.CodeGenerator

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "showNumber" $ do
    it "showNumber 0" $ do showNumber 0 `shouldBe` ""
    it "showNumber 1" $ do showNumber 1 `shouldBe` "t"
    it "showNumber 10" $ do showNumber 10 `shouldBe` "to"
    it "showNumber 11" $ do showNumber 11 `shouldBe` "ti"
    it "showNumber 110" $ do showNumber 110 `shouldBe` "atn"
    it "showNumber 111" $ do showNumber 111 `shouldBe` "ats"
    it "showNumber 1110" $ do showNumber 1110 `shouldBe` "otii"
    it "showNumber 1111" $ do showNumber 1111 `shouldBe` "otin"
    it "showNumber 11110" $ do showNumber 11110 `shouldBe` "iiant"
    it "showNumber 11111" $ do showNumber 11111 `shouldBe` "iiana"
    it "showNumber 111110" $ do showNumber 111110 `shouldBe` "sitsos"
    it "showNumber 111111" $ do showNumber 111111 `shouldBe` "sitsih"

  describe "Examples" $ do
    it "Hello, Wolrd!" $ do codeGeneration helloIL `shouldBe` helloETA

    it "Copying Input to Output" $ do codeGeneration pipIL' `shouldBe` pipETA

