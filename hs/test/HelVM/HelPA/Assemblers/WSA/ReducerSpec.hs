module HelVM.HelPA.Assemblers.WSA.ReducerSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.Reducer
import           HelVM.HelPA.Assemblers.WSA.TestData

import           Test.Hspec                          (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "reduce" $
    forM_ [ ("io"     , ioIL     , ioILReduced)
          , ("memory" , memoryIL , memoryILReduced)
          , ("prim"   , primIL   , primILReduced)
          ] $ \(fileName , il , ilReduced) ->
      it fileName $ reduce False il `shouldBe` ilReduced
