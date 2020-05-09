{-# LANGUAGE TupleSections #-}
module HelVM.HelPA.Common.Util where

import Numeric.Natural

naturalToDigits7 :: Natural -> [Natural]
naturalToDigits7 = unfoldl lambda where
  lambda :: Natural -> Maybe (Natural, Natural)
  lambda 0 = Nothing
  lambda n = Just (n `div` 7, n `mod` 7)

naturalToDigits2 :: Natural -> [Natural]
naturalToDigits2 = unfoldl lambda where
  lambda :: Natural -> Maybe (Natural, Natural)
  lambda 0 = Nothing
  lambda n = Just (n `div` 2, n `mod` 2)

charToDigits :: Char -> [Natural]
charToDigits _ = []
--charToDigits char = (ord char) `mod` 256

--intToDigits' :: Int -> Int -> [Int]
--intToDigits' _ 0 = []
--intToDigits' a b = a `div` b : charToDigits (a `mod` b) (b `div` 2)

----

unfoldl :: (a -> Maybe (a,b)) -> a -> [b]
unfoldl lambda value = check $ lambda value where
  check  Nothing     = []
  check (Just (a,b)) = unfoldl lambda a ++ [b]

toList :: ([k], v) -> [(k, v)]
toList (keys, value) = (, value) <$> keys
