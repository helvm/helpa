module HelVM.HelPA.Assemblers.Common.DSL where

import           HelVM.HelPA.Assemblers.Common.Config
import           HelVM.HelPA.Assemblers.Common.Environment

import           HelVM.HelPA.Assembler.Value

import           Control.Monad.RWS.Lazy

import qualified Data.ListLike                             as LL

calculateLabel :: MonadDSL a m => m Identifier
calculateLabel = do
  s <- get
  modify nextTextLabelCount
  pure $ show $ textLabelCount s

calculateLocalLabel :: MonadDSL a m => Identifier -> m Identifier
calculateLocalLabel label = do
  s <- get
  modify nextTextLabelCount
  pure $ label <> ":" <> show (textLabelCount s)

temp0 :: Register
temp0 = "temp0"

type Immediate = Integer

unRegister :: Register -> Text
unRegister n = n

unImmediate :: Immediate -> Integer
unImmediate i = i

type ImmediateORRegister = IntegerValue

dsl :: MonadDSL a m => a -> m ()
dsl = tell . LL.singleton

type Register = Text

type MonadDSL a m = MonadRWS Config [a] Environment m

type DSL a = RWS Config [a] Environment ()

