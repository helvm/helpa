module HelVM.HelPA.Assemblers.WSA.AssemblerSpec where

import HelVM.HelPA.Assemblers.WSA.Assembler
import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.CodeGenerator

import HelVM.HelPA.Assemblers.WSA.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "Examples" $ do
    ioEither     <- assembleLibFile "io"
    memoryEither <- assembleLibFile "memory"
    primpEither  <- assembleFile "prim"
    it "io"     $ do ioEither     `shouldBe` Right (showTL ioTL)
    it "memory" $ do memoryEither `shouldBe` Right (showTL memoryTL)
    it "prim"   $ do primpEither  `shouldBe` Right (showTL primTL)

assembleLibFile name = runIO $ assemblyIO False "src/test/resources/wsa/libs/" $ "src/test/resources/wsa/libs/" ++ name ++ ".wsa"
assembleFile name = runIO $ assemblyIO False "src/test/resources/wsa/libs/" $ "src/test/resources/wsa/examples/" ++ name ++ ".wsa"
