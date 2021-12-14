module HelVM.HelPA.Assemblers.ASQ.EsoLangs.Reducers.ItemReducer (
  reduceItemList
) where

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Line

reduceItemList :: ItemList -> LineList
reduceItemList il = accToList $ foldl' f ([],[]) il where
  accToList :: ACC -> LineList
  accToList (acc, []) =  acc
  accToList (acc, ll) =  acc <> [Line ll Nothing]
  f :: ACC -> Item -> ACC
  f (acc , ll) (ItemLabel l)      = (acc , ll <> [l])
  f (acc , ll) (ItemExpression e) = (acc <> [lineFromExpression ll e], [])
  f (acc , ll) (ItemString s)     = (acc <> lineList ll s, [])

type ACC = (LineList , LabelList)
