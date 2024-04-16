module HelVM.HelPA.Assemblers.Common.Config where

import           HelVM.HelPA.Assembler.Value

import           Control.Type.Operator
import qualified Data.List                   as List

-- A  -> Register 1 |
-- B  -> Register 2
-- L3 -> Label 3

--mov B  A # movr B  A
--mov L3 A # movi L3 A
--mov 0  A # movi 0  A

changeIntegerValue :: MonadReader Config m => IntegerValue -> m IntegerValue
changeIntegerValue (Literal value) = pure $ Literal value
changeIntegerValue (Variable name) = changeIdentifier name

changeIdentifier :: MonadReader Config m => Text -> m IntegerValue
changeIdentifier name = check <$> labelAddressOpt name where
  check :: Maybe Int -> IntegerValue
  check (Just address) = Literal $ fromIntegral address
  check Nothing        = Variable name

labelAddressOpt :: MonadReader Config m => Text -> m $ Maybe Int
labelAddressOpt name = List.elemIndex name <$> asks labels

makeConfig :: Config
makeConfig = Config 1 []

data Config = Config
  { config :: Natural
  , labels :: [Text]
  }
