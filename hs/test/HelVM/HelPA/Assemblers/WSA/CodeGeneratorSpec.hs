module HelVM.HelPA.Assemblers.WSA.CodeGeneratorSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.CodeGenerator
import HelVM.HelPA.Assemblers.WSA.Token
import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.HelPA.Assemblers.CartesianProduct
import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.AssemblyOptions

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "reduceAndGenerateCode" $ do
    forM_ ([ ("io"     , ioILReduced)
           , ("memory" , memoryILReduced)
           , ("prim"   , primILReduced <> ioILReduced)
           ] >><< manyOptionsWithName) $ \(fileName , il, name, options) -> do
      it (name </> fileName) $ do
        reduceAndGenerateCode options il `goldenShouldBe` buildAbsolutePathToWsFile ("codeGenerator" </> name </> fileName)

  describe "valueToTL" $ do
    it "valueToTL 0"  $ do valueToTL   0  `shouldBe` [S,N]
    it "valueToTL 1"  $ do valueToTL   1  `shouldBe` [S,T,N]
    it "valueToTL 2"  $ do valueToTL   2  `shouldBe` [S,T,S,N]
    it "valueToTL 3"  $ do valueToTL   3  `shouldBe` [S,T,T,N]
    it "valueToTL 4"  $ do valueToTL   4  `shouldBe` [S,T,S,S,N]
    it "valueToTL 5"  $ do valueToTL   5  `shouldBe` [S,T,S,T,N]
    it "valueToTL 6"  $ do valueToTL   6  `shouldBe` [S,T,T,S,N]
    it "valueToTL 7"  $ do valueToTL   7  `shouldBe` [S,T,T,T,N]
    it "valueToTL -0" $ do valueToTL (-0) `shouldBe` [S,N]
    it "valueToTL -1" $ do valueToTL (-1) `shouldBe` [T,T,N]
    it "valueToTL -2" $ do valueToTL (-2) `shouldBe` [T,T,S,N]
    it "valueToTL -3" $ do valueToTL (-3) `shouldBe` [T,T,T,N]
    it "valueToTL -4" $ do valueToTL (-4) `shouldBe` [T,T,S,S,N]
    it "valueToTL -5" $ do valueToTL (-5) `shouldBe` [T,T,S,T,N]
    it "valueToTL -6" $ do valueToTL (-6) `shouldBe` [T,T,T,S,N]
    it "valueToTL -7" $ do valueToTL (-7) `shouldBe` [T,T,T,T,N]

  describe "stringToTL" $ do
    it "stringToTL \" \"" $ do stringToTL " " `shouldBe` [S,S,T,S,S,S,S,S, N]
    it "stringToTL \"A\"" $ do stringToTL "A" `shouldBe` [S,T,S,S,S,S,S,T, N]
    it "stringToTL \"Z\"" $ do stringToTL "Z" `shouldBe` [S,T,S,T,T,S,T,S, N]
    it "stringToTL \"a\"" $ do stringToTL "a" `shouldBe` [S,T,T,S,S,S,S,T, N]
    it "stringToTL \"z\"" $ do stringToTL "z" `shouldBe` [S,T,T,T,T,S,T,S, N]
