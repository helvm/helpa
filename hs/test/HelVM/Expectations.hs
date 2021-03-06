{-# LANGUAGE UndecidableInstances #-}
module HelVM.Expectations  (
  goldenShouldSafeExceptT,
  goldenShouldSafe,
  goldenShouldSafeReturn,
  goldenShouldReturn,
  goldenShouldBe,
  ioShouldSafe,
  shouldSafeExceptT,
  shouldSafe,
  ioShouldBe,
  shouldSafeReturn,
  shouldBeDo,
) where

import HelVM.Common.Safe
import HelVM.Common.Util

import Control.Type.Operator
import System.FilePath.Posix

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.Hspec.Golden
import Test.Hspec.Core.Spec

infix 1 `goldenShouldSafeExceptT`
goldenShouldSafeExceptT:: (Typeable a , Show a) => SafeExceptT IO a -> FilePath -> WrappedGoldenIO Text
goldenShouldSafeExceptT actualOutput = goldenShouldReturn (exceptTToIO actualOutput)

infix 1 `goldenShouldSafe`
goldenShouldSafe :: (Typeable a , Show a) => Safe a -> FilePath -> WrappedGoldenIO Text
goldenShouldSafe actualOutput = goldenShouldReturn (safeToIO actualOutput)

infix 1 `goldenShouldSafeReturn`
goldenShouldSafeReturn :: (Typeable a , Show a) => IO (Safe a) -> FilePath -> WrappedGoldenIO Text
goldenShouldSafeReturn = goldenShouldReturn . safeIOToIO

infix 1 `goldenShouldReturn`
goldenShouldReturn :: (Typeable a , Show a) => IO a -> FilePath -> WrappedGoldenIO Text
goldenShouldReturn actualOutput fileName = WrappedGoldenIO $ flip goldenShouldBe fileName <$> actualOutput

infix 1 `goldenShouldBe`
goldenShouldBe :: (Typeable a , Show a) => a -> FilePath -> Golden Text
goldenShouldBe actualOutput fileName =
  Golden {
    output = showToText actualOutput,
    encodePretty = show,
    writeToFile = writeFileText,
    readFromFile = readFileText,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

----

infix 1 `ioShouldSafe`
ioShouldSafe :: (Show a , Eq a) => IO (Safe a) -> IO a -> Expectation
ioShouldSafe action expected = join $ liftA2 shouldSafe action expected

infix 1 `shouldSafe`
shouldSafe :: (Show a , Eq a) => Safe a -> a -> Expectation
shouldSafe action = shouldParse (safeToLegacySafe action)

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack , Show a , Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

----

infix 1 `shouldSafeExceptT`
shouldSafeExceptT :: (Show a , Eq a) => SafeExceptT IO a -> a -> Expectation
shouldSafeExceptT action = shouldReturn (exceptTToIO action)


infix 1 `shouldSafeReturn`
shouldSafeReturn :: (Show a , Eq a) => IO (Safe a) -> a -> Expectation
shouldSafeReturn action = shouldReturn (safeIOToIO action)

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack , Show a , Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected

----

newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }

type GoldenIO a = IO $ Golden a

instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
