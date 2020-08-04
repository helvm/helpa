module HelVM.HelPA.Common.OrError where

import Text.Read

import qualified Data.Map.Strict as Map

readOrError :: (Read a) => String -> a
readOrError raw = check $ readEither raw where
  check (Right result) = result
  check (Left message)  = error $ message ++ " [" ++ raw ++ "]"

findOrError :: (Show k, Ord k, Show v) => k -> Map.Map k v -> v
findOrError key hash = check $ Map.lookup key hash where
  check (Just result) = result
  check  Nothing      = error $ "key [" ++ show key ++ "] map {" ++ show hash ++ "}"
  