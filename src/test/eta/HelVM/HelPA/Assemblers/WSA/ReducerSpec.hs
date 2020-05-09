{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.WSA.ReducerSpec where

import HelVM.HelPA.Assemblers.WSA.Reducer
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assemblers.WSA.TestData

import qualified Data.Text as T

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    it "io"     $ do reduce ioIL     `shouldBe` ioIL'
    it "memory" $ do reduce memoryIL `shouldBe` memoryIL'
    it "prim"   $ do reduce primIL   `shouldBe` primIL'
