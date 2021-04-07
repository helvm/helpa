module HelVM.HelPA.Assemblers.WSA.LinkerSpec where

import HelVM.HelPA.Assemblers.WSA.Linker
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assemblers.WSA.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    ioEither     <- linkLibFile "io"
    memoryEither <- linkLibFile "memory"
    primEither   <- linkFile    "prim"

    it "io"     $ do ioEither     `shouldBe` Right ioIL
    it "memory" $ do memoryEither `shouldBe` Right memoryIL
    it "prim"   $ do primEither   `shouldBe` Right (primIL ++ ioIL)

linkLibFile name = runIO $ linkLibIO "examples/wsa/libs/" $ name ++ ".wsa"
linkFile name = runIO $ linkIO "examples/wsa/libs/" $ "examples/wsa/examples/" ++ name ++ ".wsa"
