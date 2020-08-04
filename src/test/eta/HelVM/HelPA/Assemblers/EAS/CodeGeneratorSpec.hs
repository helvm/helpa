module HelVM.HelPA.Assemblers.EAS.CodeGeneratorSpec where

import HelVM.HelPA.Assemblers.EAS.CodeGenerator

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

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
    it "true"     $ do generateCode trueIL      `shouldBe` trueETA
    it "hello"    $ do generateCode helloIL     `shouldBe` helloETA
    it "pip"      $ do generateCode pipIL'      `shouldBe` pipETA
    it "pip2"     $ do generateCode pip2IL'     `shouldBe` pip2ETA
    it "reverse"  $ do generateCode reverseIL'  `shouldBe` reverseETA
    it "function" $ do generateCode functionIL  `shouldBe` functionETA
    it "add"      $ do generateCode addIL'      `shouldBe` addETA
    it "writestr" $ do generateCode writeStrIL' `shouldBe` writeStrETA
    it "hello2"   $ do generateCode hello2IL'   `shouldBe` hello2ETA
    it "writenum" $ do generateCode writeNumIL' `shouldBe` writeNumETA
    it "multiply" $ do generateCode multiplyIL' `shouldBe` multiplyETA
    it "readnum"  $ do generateCode readNumIL'  `shouldBe` readNumETA
    it "fact"     $ do generateCode factIL'     `shouldBe` factETA
--    it "bottles"  $ do generateCode bottlesIL'  `shouldBe` bottlesETA
    it "euclid"   $ do generateCode euclidIL'   `shouldBe` euclidETA
