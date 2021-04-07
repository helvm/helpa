module HelVM.HelPA.Assemblers.EAS.ReducerSpec where

import HelVM.HelPA.Assemblers.EAS.Reducer
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do 
  describe "Examples" $ do
    it "true"     $ do reduce trueIL           `shouldBe` trueIL
    it "hello"    $ do reduce helloIL          `shouldBe` helloIL
    it "pip"      $ do reduce pipIL            `shouldBe` pipILReduced
    it "pip2"     $ do reduce pip2IL           `shouldBe` pip2ILReduced
    it "reverse"  $ do reduce reverseIL        `shouldBe` reverseILReduced
    it "function" $ do reduce functionIL       `shouldBe` functionIL
    it "add"      $ do reduce addILLinked      `shouldBe` addILReduced
    it "writestr" $ do reduce writeStrIL       `shouldBe` writeStrILReduced
    it "hello2"   $ do reduce hello2ILLinked   `shouldBe` hello2ILReduced
    it "hello4"   $ do reduce hello4ILLinked   `shouldBe` hello2ILReduced
    it "writenum" $ do reduce writeNumILLinked `shouldBe` writeNumILReduced
    it "multiply" $ do reduce multiplyIL       `shouldBe` multiplyILReduced
    it "readnum"  $ do reduce readNumILLinked  `shouldBe` readNumILReduced
    it "fact"     $ do reduce factILLinked     `shouldBe` factILReduced
    it "bottles"  $ do reduce bottlesILLinked  `shouldBe` bottlesILReduced
    it "euclid"   $ do reduce euclidIL         `shouldBe` euclidILReduced
