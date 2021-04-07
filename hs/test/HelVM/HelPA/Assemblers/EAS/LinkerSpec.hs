module HelVM.HelPA.Assemblers.EAS.LinkerSpec where

import HelVM.HelPA.Assemblers.EAS.Linker
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    trueEither     <- linkFile "true"
    helloEither    <- linkFile "hello"
    pipEither      <- linkFile "pip"
    pip2Either     <- linkFile "pip2"
    reverseEither  <- linkFile "reverse"
    functionEither <- linkFile "function"
    addEither      <- linkFile "add"
    writeStrEither <- linkFile "writestr"
    hello2Either   <- linkFile "hello2"
    hello3Either   <- linkFile "hello3"
    hello4Either   <- linkFile "hello4"
    writeNumEither <- linkFile "writenum"
    multiplyEither <- linkFile "multiply"
    readNumEither  <- linkFile "readnum"
    factEither     <- linkFile "fact"
    bottlesEither  <- linkFile "bottles"
    euclidEither   <- linkFile "euclid"

    it "true"     $ do trueEither     `shouldBe` Right trueIL
    it "hello"    $ do helloEither    `shouldBe` Right helloIL
    it "pip"      $ do pipEither      `shouldBe` Right pipIL
    it "pip2"     $ do pip2Either     `shouldBe` Right pip2IL
    it "reverse"  $ do reverseEither  `shouldBe` Right reverseIL
    it "function" $ do functionEither `shouldBe` Right functionIL
    it "writestr" $ do writeStrEither `shouldBe` Right writeStrIL
    it "hello2"   $ do hello2Either   `shouldBe` Right (hello2IL ++ writeStrIL)
    it "hello3"   $ do hello3Either   `shouldBe` Right (hello2IL ++ writeStrIL)
    it "hello4"   $ do hello4Either   `shouldBe` Right (hello4IL ++ writeStrIL)
    it "writenum" $ do writeNumEither `shouldBe` Right writeNumIL
    it "multiply" $ do multiplyEither `shouldBe` Right multiplyIL
    it "readnum"  $ do readNumEither  `shouldBe` Right readNumIL
    it "fact"     $ do factEither     `shouldBe` Right (factIL ++ readNumIL ++ writeNumIL ++ multiplyIL ++ writeStrIL)
--    it "bottles"  $ do bottlesEither  `shouldBe` Right bottlesIL
    it "euclid"   $ do euclidEither   `shouldBe` Right euclidIL

linkFile name = runIO $ linkLibIO "examples/eas/" $ name ++ ".eas"
