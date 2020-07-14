module HelVM.HelPA.Common.Util where

import Numeric.Natural
import Text.Read

import qualified Data.Map.Strict as Map

readOrError :: (Read a) => String -> a
readOrError v = check $ readEither v where
  check (Right r) = r
  check (Left l) = error $ l ++ " [" ++ v ++ "]"

naturalTo7Digits :: Natural -> [Int]
naturalTo7Digits = unfoldl l where
  l :: Natural -> Maybe (Natural, Int)
  l 0 = Nothing
  l n = Just (n `div` 7, fromIntegral (n `mod` 7))

----

unfoldl :: (b -> Maybe (b,a)) -> b -> [a]
unfoldl f value = l $ f value where
  l  Nothing     = []
  l (Just (a,b)) = unfoldl f a ++ [b]

toList :: ([k], v) -> [(k, v)]
toList (keys, value) = map (\key -> (key, value)) keys

----

findOrError :: (Show k, Ord k, Show v) => k -> Map.Map k v -> v
findOrError key hash = match $ Map.lookup key hash where
  match (Just result) = result
  match  Nothing      = error $ " [" ++ (show key) ++ "] {" ++ (show hash) ++ "}"
