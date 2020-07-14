module HelVM.HelPA.Assemblers.EAS.TranslatorSpec where

import HelVM.HelPA.Assemblers.EAS.Translator
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do 
  describe "Examples" $ do
    it "Copying Input to Output" $ do  translate pipIL `shouldBe` pipIL'
