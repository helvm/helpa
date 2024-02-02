module HelVM.HelPA.Assemblers.Common.DSL where

import           Control.Monad.RWS.Lazy
import           Data.DList

type Config = ()

type Environment = ()

type DSL a = RWS Config (DList a) Environment ()

