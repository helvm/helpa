module HelVM.HelPA.Assemblers.Common.DSL where

import           HelVM.HelPA.Assembler.Value

import           Control.Monad.RWS.Lazy

import qualified Data.ListLike               as LL

newtype Register = R Text

calculateLabel :: MonadDSL a m => m Identifier
calculateLabel = do
  label <- get
  modify (+ 1)
  pure $ show label

calculateLocalLabel :: MonadDSL a m => Identifier -> m Identifier
calculateLocalLabel label = do
  suffix <- get
  modify (+ 1)
  pure $ label <> ":" <> show suffix

acc :: Register
acc = R ""

acc1 :: Register
acc1 = R "1"

newtype Immediate = I Integer

unRegister :: Register -> Text
unRegister (R n) = n

unImmediate :: Immediate -> Integer
unImmediate (I i) = i

type ImmediateORRegister = Either Immediate Register

dsl :: MonadDSL a m => a -> m ()
dsl = tell . LL.singleton

type Config = Natural

type Environment = Natural

type MonadDSL a m = MonadRWS Config [a] Environment m

type DSL a = RWS Config [a] Environment ()

