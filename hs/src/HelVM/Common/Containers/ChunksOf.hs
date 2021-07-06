module HelVM.Common.Containers.ChunksOf where

class ChunksOf e c | c -> e where
  chunksOf :: Int -> c -> c
