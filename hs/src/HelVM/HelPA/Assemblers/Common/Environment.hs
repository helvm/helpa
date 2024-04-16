module HelVM.HelPA.Assemblers.Common.Environment where

import qualified Data.List as List

registerAddress :: MonadState Environment m => Text -> m Int
registerAddress = fix registerAddressFix

registerAddressFix :: MonadState Environment m => (Text -> m Int) -> Text -> m Int
registerAddressFix rec name = maybe (modify (addRegister name) >> rec name) pure . List.elemIndex name =<< gets registerNames

addRegister :: Text -> Environment -> Environment
addRegister name e = e { registerNames = registerNames e <> [name] }

nextTextLabelCount :: Environment -> Environment
nextTextLabelCount e = e { textLabelCount = textLabelCount e + 1 }

nextDataLabelCount :: Environment -> Environment
nextDataLabelCount e = e { dataLabelCount = dataLabelCount e + 1 }

makeEnvironment :: Environment
makeEnvironment = Environment 1 0 []

data Environment = Environment
  { textLabelCount :: Natural
  , dataLabelCount :: Natural
  , registerNames  :: [Text]
  }
