module HelVM.HelPA.Assemblers.EAS.ReducerSpec where

import HelVM.HelPA.Assemblers.EAS.Reducer
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do 
  describe "Examples" $ do
    it "true"     $ do reduce trueIL     `shouldBe` trueIL
    it "hello"    $ do reduce helloIL    `shouldBe` helloIL
    it "pip"      $ do reduce pipIL      `shouldBe` pipIL'
    it "pip2"     $ do reduce pip2IL     `shouldBe` pip2IL'
    it "reverse"  $ do reduce reverseIL  `shouldBe` reverseIL'
    it "function" $ do reduce functionIL `shouldBe` functionIL
    it "add"      $ do reduce (addIL ++ functionIL) `shouldBe` addIL'
    it "writestr" $ do reduce writeStrIL `shouldBe` writeStrIL'
    it "hello2"   $ do reduce (hello2IL ++ writeStrIL) `shouldBe` hello2IL'
    it "hello4"   $ do reduce (hello4IL ++ writeStrIL)   `shouldBe` hello2IL'
    it "writenum" $ do reduce (writeNumIL ++ writeStrIL) `shouldBe` writeNumIL'
    it "multiply" $ do reduce multiplyIL `shouldBe` multiplyIL'
    it "readnum"  $ do reduce (readNumIL ++ multiplyIL)  `shouldBe` readNumIL'
    it "fact"     $ do reduce (factIL ++ readNumIL ++ writeNumIL ++ multiplyIL ++ writeStrIL) `shouldBe` factIL'
--    it "bottles"  $ do reduce bottlesIL  `shouldBe` bottlesIL
    it "euclid"   $ do reduce euclidIL   `shouldBe` euclidIL'
