module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    trueEither     <- assemblyFile "true"
    helloEither    <- assemblyFile "hello"
    pipEither      <- assemblyFile "pip"
    pip2Either     <- assemblyFile "pip2"
    reverseEither  <- assemblyFile "reverse"
    functionEither <- assemblyFile "function"
    addEither      <- assemblyFile "add"
    writeStrEither <- assemblyFile "writestr"
    hello2Either   <- assemblyFile "hello2"
    hello3Either   <- assemblyFile "hello3"
    hello4Either   <- assemblyFile "hello4"
    writeNumEither <- assemblyFile "writenum"
    multiplyEither <- assemblyFile "multiply"
    readNumEither  <- assemblyFile "readnum"
    factEither     <- assemblyFile "fact"
    bottlesEither  <- assemblyFile "bottles"
    euclidEither   <- assemblyFile "euclid"

    it "true"     $ do trueEither     `shouldBe` Right trueETA
    it "hello"    $ do helloEither    `shouldBe` Right helloETA
    it "pip"      $ do pipEither      `shouldBe` Right pipETA
    it "pip2"     $ do pip2Either     `shouldBe` Right pip2ETA
    it "reverse"  $ do reverseEither  `shouldBe` Right reverseETA
    it "function" $ do functionEither `shouldBe` Right functionETA
    it "writestr" $ do writeStrEither `shouldBe` Right writeStrETA
    it "hello2"   $ do hello2Either   `shouldBe` Right hello2ETA
    it "hello3"   $ do hello3Either   `shouldBe` Right hello2ETA
    it "hello4"   $ do hello4Either   `shouldBe` Right hello2ETA
--    it "writenum" $ do writeNumEither `shouldBe` Right writeNumETA
    it "multiply" $ do multiplyEither `shouldBe` Right multiplyETA
--    it "readnum"  $ do readNumEither  `shouldBe` Right readNumETA
    it "fact"     $ do factEither     `shouldBe` Right factETA
    it "bottles"  $ do bottlesEither  `shouldBe` Right bottlesETA
    it "euclid"   $ do euclidEither   `shouldBe` Right euclidETA

assemblyFile name = runIO $ assemblyIO "src/test/resources/eas/" $ "src/test/resources/eas/" ++ name ++ ".eas"
