module HelVM.Common.Digit.ToDigit (
  makeDigitString,
  makeAsciiString28,
  makeAsciiString,
  makeIntegral2,
  makeIntegral,
  ToDigit,
  toDigit,
) where

import           HelVM.Common.Digit.Digits

import           HelVM.Common.Collections.SList
import           HelVM.Common.Safe

import           Data.Char                      hiding (chr)

import qualified HelVM.Common.Collections.SList as SList

import qualified Data.ListLike                  as LL

makeDigitString :: (MonadSafe m , ToDigit a) => SList a -> m SString
makeDigitString = traverse toDigitChar

makeAsciiString28 :: (MonadSafe m , ToDigit a) => SList a -> m SString
makeAsciiString28 = makeAsciiString 2 8

makeAsciiString :: (MonadSafe m , ToDigit a) => Int -> Int -> SList a -> m SString
makeAsciiString base n xs = traverse (makeChar base) $ SList.chunksOf n xs

makeChar :: (MonadSafe m , ToDigit a) => Int -> SList a -> m Char
makeChar base xs = chr <$> makeIntegral base (LL.reverse xs)

toDigitChar :: MonadSafe m => ToDigit a => a -> m Char
toDigitChar a = integerToDigit <$> toDigit a

integerToDigit :: Integer -> Char
integerToDigit = intToDigit . fromInteger

makeIntegral2 :: (MonadSafe m , ToDigit a , Integral b) => SList a -> m b
makeIntegral2 = makeIntegral 2

makeIntegral :: (MonadSafe m , ToDigit a , Integral b) => b -> SList a -> m b
makeIntegral base digits = digitsToIntegral base (toDigit <$> digits)

class ToDigit t where
  toDigit :: (MonadSafe m, Integral a) => t -> m a
