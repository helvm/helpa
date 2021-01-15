{-# LANGUAGE TupleSections #-}
module HelVM.HelPA.Common.Util where

naturalToDigits7 :: Natural -> [Natural]
naturalToDigits7 = naturalToDigits 7

naturalToDigits2 :: Natural -> [Natural]
naturalToDigits2 = naturalToDigits 2

naturalToDigits :: Natural -> Natural -> [Natural]
naturalToDigits b = unfoldl lambda where
  lambda :: Natural -> Maybe (Natural, Natural)
  lambda 0 = Nothing
  lambda n = Just (n `div` b, n `mod` b)

----

unfoldl :: (a -> Maybe (a,b)) -> a -> [b]
unfoldl lambda value = check $ lambda value where
  check  Nothing     = []
  check (Just (a,b)) = unfoldl lambda a ++ [b]

setDefault :: ([k], v) -> [(k, v)]
setDefault (keys, value) = (, value) <$> keys
