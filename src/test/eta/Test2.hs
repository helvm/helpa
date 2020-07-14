{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Applicative
import Control.Exception (evaluate)
import Data.Attoparsec.Text
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck

import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "shouldParse" $
    it "works on: \"x\" ~> char 'x'" $
      ("x" :: T.Text) ~> char 'x'
        `shouldParse` 'x'

  describe "parseSatisfies" $ do
    it "works on: \"x\" and (=='x')" $
      ("x" :: T.Text) ~> char 'x'
        `parseSatisfies` (=='x')

    it "\">>>\" satisfies length == 3 when parser as a list of char" $
      (">>>" :: T.Text) ~> many (char '>')
        `parseSatisfies` ((==3) . Prelude.length)

  describe "shouldFailOn" $
    it "char 'x' fails on \"ha\"" $
      char 'x' `shouldFailOn` ("ha" :: T.Text)

  describe "shouldSucceedOn" $
    it "char 'x' succeeds on \"x\"" $
      char 'x' `shouldSucceedOn` ("x" :: T.Text)

  describe "leavesUnconsumed" $
--    it "works on \"xa\" ~?> char 'x'" $
--      ("xa" :: T.Text) ~?> char 'x'
--        `leavesUnconsumed` "a"

    it "char 'x' leaves nothing unconsumed on \"x\"" $
      ("x" :: T.Text) ~?> char 'x'
        `leavesUnconsumed` ""