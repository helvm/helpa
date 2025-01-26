module HelVM.HelPA.Assemblers.Frontend.WSA.CodeGeneratorSpec (spec) where

import           HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptionsExtra
import           HelVM.HelPA.Assemblers.Backend.WSA.CodeGenerator
import           HelVM.HelPA.Assemblers.Backend.WSA.Token
import           HelVM.HelPA.Assemblers.Frontend.WSA.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.WSA.TestDataReduced

import           HelVM.HelIO.CartesianProduct
import           HelVM.HelIO.NamedValue

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                              (Spec, describe, it)

spec :: Spec
spec = do
  describe "reduceAndGenerateCode" $
    forM_ ([ ("io"     , ioILReduced)
           , ("memory" , memoryILReduced)
           , ("prim"   , primILReduced <> ioILReduced)
           ] >>*< manyOptionsWithName) $ \(fileName , il , namedOptions) ->
      it (name namedOptions </> fileName) $
        generateCode2 (value namedOptions) il `goldenShouldSafe` buildAbsolutePathToWsFile ("codeGenerator" </> name namedOptions </> fileName)

  describe "valueToTL" $
    forM_ [ ( 0 , [S,N])
          , ( 1 , [S,T,N])
          , ( 2 , [S,T,S,N])
          , ( 3 , [S,T,T,N])
          , ( 4 , [S,T,S,S,N])
          , ( 5 , [S,T,S,T,N])
          , ( 6 , [S,T,T,S,N])
          , ( 7 , [S,T,T,T,N])
          , (-0 , [S,N])
          , (-1 , [T,T,N])
          , (-2 , [T,T,S,N])
          , (-3 , [T,T,T,N])
          , (-4 , [T,T,S,S,N])
          , (-5 , [T,T,S,T,N])
          , (-6 , [T,T,T,S,N])
          , (-7 , [T,T,T,T,N])
          ] $ \(line , il) ->
      it (show line) $ valueToTL line `shouldSafe` il

  describe "identifierToTL" $
    forM_ [ (" " , [S,S,T,S,S,S,S,S , N])
          , ("A" , [S,T,S,S,S,S,S,T , N])
          , ("Z" , [S,T,S,T,T,S,T,S , N])
          , ("a" , [S,T,T,S,S,S,S,T , N])
          , ("z" , [S,T,T,T,T,S,T,S , N])
          ] $ \(line , il) ->
      it line $ identifierToTL (toText line) `shouldSafe` il
