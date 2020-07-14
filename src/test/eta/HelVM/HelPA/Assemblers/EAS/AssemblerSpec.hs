{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler

import HelVM.HelPA.Assemblers.EAS.TestData

import qualified Data.Text as T

import Test.Hspec

spec :: Spec
spec = do
  describe "Examples" $ do
    it "Hello, Wolrd!" $ do assembly helloEAS `shouldBe` Right helloETA

    it "Copying Input to Output" $ do assembly pipEAS `shouldBe` Right pipETA

    it "Function Call and Function Def " $ do
      assembly (T.concat [functionCallEAS,functionDefinitionEAS]) `shouldBe` Right functionCallAndDefETA

    it "A Better Implementation of Hello, World!" $ do
      assembly (T.concat [hello2EAS,writingAStringEAS]) `shouldBe` Right hello2ETA

    it "A Char Better Implementation of Hello, World!" $ do
      assembly (T.concat [charBetterHelloWorldEAS,writingAStringEAS]) `shouldBe` Right hello2ETA

    it "A String Better Implementation of Hello, World!" $ do
      assembly (T.concat [stringBetterHelloWorldEAS,writingAStringEAS]) `shouldBe` Right hello2ETA

    it "A Third Better Implementation of Hello, World!" $ do
      assembly (T.concat [stringBetterHelloWorldEAS,writingAStringEAS]) `shouldBe` Right hello2ETA

