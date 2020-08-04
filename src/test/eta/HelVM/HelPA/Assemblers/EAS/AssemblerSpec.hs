{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import qualified Data.Text as T

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    trueEither     <- parseFromFile "true"
    helloEither    <- parseFromFile "hello"
    pipEither      <- parseFromFile "pip"
    pip2Either     <- parseFromFile "pip2"
    reverseEither  <- parseFromFile "reverse"
    functionEither <- parseFromFile "function"
    addEither      <- parseFromFile "add"
    writeStrEither <- parseFromFile "writeStr"
    hello2Either   <- parseFromFile "hello2"
    hello3Either   <- parseFromFile "hello3"
    hello4Either   <- parseFromFile "hello4"
    writeNumEither <- parseFromFile "writenum"
    multiplyEither <- parseFromFile "multiply"
    readNumEither  <- parseFromFile "readnum"
    factEither     <- parseFromFile "fact"
    bottlesEither  <- parseFromFile "bottles"
    euclidEither   <- parseFromFile "euclid"

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

parseFromFile name = runIO $ assemblyIO "src/test/resources/eas/" $ name ++ ".eas"
