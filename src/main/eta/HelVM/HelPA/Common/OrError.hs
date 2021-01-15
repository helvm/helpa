module HelVM.HelPA.Common.OrError where

import Relude.Extra

readOrError :: Read a => String -> a
readOrError raw = check $ readEither raw where
  check (Right result) = result
  check (Left message) = error $ message <> " [" <> toText raw <> "]"

findOrError :: (Show k, Ord k, Show v) => k -> Map k v -> v
findOrError key hash = check $ lookup key hash where
  check (Just result) = result
  check  Nothing      = error $ "key [" <> show key <> "] map {" <> show hash <> "}"
