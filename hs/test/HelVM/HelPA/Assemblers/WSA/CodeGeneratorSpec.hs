module HelVM.HelPA.Assemblers.WSA.CodeGeneratorSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.WSA.FileUtil
import           HelVM.HelPA.Assemblers.WSA.TestData
import           HelVM.HelPA.Assemblers.WSA.Token

import           HelVM.HelPA.Assemblers.AssemblyOptionsUtil

import           HelVM.Common.NamedValue

import           HelVM.CartesianProduct
import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                 (Spec, describe, it)

spec :: Spec
spec = do
  describe "reduceAndGenerateCode" $ do
    forM_ ([ ("io"     , ioILReduced)
           , ("memory" , memoryILReduced)
           , ("prim"   , primILReduced <> ioILReduced)
           ] >><| manyOptionsWithName) $ \(fileName , il , namedOptions) -> do
      it (name namedOptions </> fileName) $ do
        reduceAndGenerateCode (value namedOptions) il `goldenShouldSafe` buildAbsolutePathToWsFile ("codeGenerator" </> name namedOptions </> fileName)

  describe "valueToTL" $ do
    it "valueToTL 0"  $ do valueToTL   0  `shouldSafe` [S,N]
    it "valueToTL 1"  $ do valueToTL   1  `shouldSafe` [S,T,N]
    it "valueToTL 2"  $ do valueToTL   2  `shouldSafe` [S,T,S,N]
    it "valueToTL 3"  $ do valueToTL   3  `shouldSafe` [S,T,T,N]
    it "valueToTL 4"  $ do valueToTL   4  `shouldSafe` [S,T,S,S,N]
    it "valueToTL 5"  $ do valueToTL   5  `shouldSafe` [S,T,S,T,N]
    it "valueToTL 6"  $ do valueToTL   6  `shouldSafe` [S,T,T,S,N]
    it "valueToTL 7"  $ do valueToTL   7  `shouldSafe` [S,T,T,T,N]
    it "valueToTL -0" $ do valueToTL (-0) `shouldSafe` [S,N]
    it "valueToTL -1" $ do valueToTL (-1) `shouldSafe` [T,T,N]
    it "valueToTL -2" $ do valueToTL (-2) `shouldSafe` [T,T,S,N]
    it "valueToTL -3" $ do valueToTL (-3) `shouldSafe` [T,T,T,N]
    it "valueToTL -4" $ do valueToTL (-4) `shouldSafe` [T,T,S,S,N]
    it "valueToTL -5" $ do valueToTL (-5) `shouldSafe` [T,T,S,T,N]
    it "valueToTL -6" $ do valueToTL (-6) `shouldSafe` [T,T,T,S,N]
    it "valueToTL -7" $ do valueToTL (-7) `shouldSafe` [T,T,T,T,N]

  describe "identifierToTL" $ do
    it "identifierToTL \" \"" $ do identifierToTL " " `shouldSafe` [S,S,T,S,S,S,S,S , N]
    it "identifierToTL \"A\"" $ do identifierToTL "A" `shouldSafe` [S,T,S,S,S,S,S,T , N]
    it "identifierToTL \"Z\"" $ do identifierToTL "Z" `shouldSafe` [S,T,S,T,T,S,T,S , N]
    it "identifierToTL \"a\"" $ do identifierToTL "a" `shouldSafe` [S,T,T,S,S,S,S,T , N]
    it "identifierToTL \"z\"" $ do identifierToTL "z" `shouldSafe` [S,T,T,T,T,S,T,S , N]
