{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelPA.Assemblers.Expectations (
  goldenShouldParseReturn,
  goldenShouldReturn,
  goldenShouldBe,
  ioShouldParse,
  ioShouldBe,
  shouldParseReturn,
  shouldBeDo
) where

import HelVM.HelPA.Common.API

import Data.Typeable

import System.FilePath.Posix

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.Golden
import Test.Hspec.Core.Spec

infix 1 `goldenShouldParseReturn`
goldenShouldParseReturn :: (Typeable a, Show a) => ParsedIO a -> String -> WrappedGoldenIO String
--goldenShouldParseReturn :: (Typeable a, Show a) => ParsedIO a -> String -> GoldenIO String
goldenShouldParseReturn = goldenShouldReturn . joinEitherToIO

infix 1 `goldenShouldReturn`
goldenShouldReturn :: (Typeable a, Show a) => IO a -> String -> WrappedGoldenIO String
--goldenShouldReturn :: (Typeable a, Show a) => IO a -> String -> GoldenIO String
goldenShouldReturn actualOutput fileName = WrappedGoldenIO $ flip goldenShouldBe fileName <$> actualOutput
--goldenShouldReturn actualOutput fileName = flip goldenShouldBe fileName <$> actualOutput

infix 1 `goldenShouldBe`
goldenShouldBe :: (Typeable a, Show a) => a -> String -> Golden String
goldenShouldBe actualOutput fileName =
  Golden {
    output = fromMaybe (show actualOutput) (cast actualOutput :: Maybe String),
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

----

infix 1 `ioShouldParse`
ioShouldParse :: (Show a, Eq a) => ParsedIO a -> IO a -> Expectation
ioShouldParse action expected = join $ liftA2 shouldParse action expected

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack, Show a, Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

----

infix 1 `shouldParseReturn`
shouldParseReturn :: (Show a, Eq a) => ParsedIO a -> a -> Expectation
shouldParseReturn action = shouldReturn (joinEitherToIO action)

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected

----

joinEitherToIO :: ParsedIO a -> IO a
joinEitherToIO io = eitherToIO =<< io

eitherToIO :: Parsed a -> IO a
eitherToIO (Right value)  = pure value
eitherToIO (Left message) = fail message

----

newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }

type GoldenIO a = IO (Golden a)

instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback

--instance Eq str => Example (GoldenIO str) where
--  type Arg (GoldenIO str) = ()
--  evaluateExample wrapped params action callback = evaluateExample' =<< wrapped where
--    evaluateExample' golden = evaluateExample golden params action callback
