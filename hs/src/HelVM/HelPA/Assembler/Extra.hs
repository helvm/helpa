--{-# LANGUAGE TupleSections #-}
module HelVM.HelPA.Assembler.Extra where

import           HelVM.HelPA.Assembler.Value

type WithAddress a = (a, Address)

withAddresses:: [a] -> [WithAddress a]
withAddresses l = zip l [0..]

type WithSymbol a = (a, Symbol)

withSymbols :: [a] -> [WithSymbol a]
withSymbols l = zip l [0..]

type LabelAddresses = Map Identifier Address
type LabelSymbols = Map Identifier Symbol

type SymbolList = [Symbol]

type Address = Natural
type Symbol = Integer

-- k :: Label
-- v :: Symbol

flippedToMapFromLists :: Ord k => [v] -> [[k]] -> Map k v
flippedToMapFromLists = flip toMapFromLists

toMapFromLists :: Ord k => [[k]] -> [v] -> Map k v
toMapFromLists kss vs = toMap $ zip kss vs

toMap :: Ord k => [([k] , v)] -> Map k v
toMap l = fromList $ setDefault =<< l

setDefault :: ([k] , v) -> [(k , v)]
setDefault (keys , value) = (, value) <$> keys

----

slipr :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr f b c a = f a b c

slipl :: (a -> b -> c -> d) -> c -> a -> b -> d
slipl f c a b = f a b c
