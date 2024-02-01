module HelVM.HelPA.Assemblers.Frontend.WSA.ReducerSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.WSA.Reducer
import           HelVM.HelPA.Assemblers.Frontend.WSA.TestData
import           HelVM.HelPA.Assemblers.Frontend.WSA.TestDataReduced

import           Test.Hspec                                          (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "reduce" $
    forM_ [ ("io"     , ioIL     , ioILReduced)
          , ("memory" , memoryIL , memoryILReduced)
          , ("prim"   , primIL   , primILReduced)
          ] $ \(fileName , il , ilReduced) ->
      it fileName $ reduce False il `shouldBe` ilReduced
