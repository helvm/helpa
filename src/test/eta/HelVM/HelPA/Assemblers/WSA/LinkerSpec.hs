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

linkLibFile name = runIO $ linkLibIO "src/test/resources/wsa/libs/" $ name ++ ".wsa"
linkFile name = runIO $ linkIO "src/test/resources/wsa/libs/" $ "src/test/resources/wsa/examples/" ++ name ++ ".wsa"
