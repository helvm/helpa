module HelVM.HelPA.Assemblers.Backend.EAS.CodeGeneratorSpec (spec) where

import           HelVM.HelPA.Assemblers.Backend.EAS.CodeGenerator
import           HelVM.HelPA.Assemblers.Backend.EAS.FileExtra
import           HelVM.HelPA.Assemblers.Backend.EAS.TestData

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  describe "generateCode" $
    forM_ [ ("true"     , trueIL)
          , ("pip"      , pipILReduced)
          , ("pip2"     , pip2ILReduced)
          , ("reverse"  , reverseILReduced)
          , ("function" , functionIL)
          , ("add"      , addILReduced)
          , ("writestr" , writeStrILReduced)
          , ("hello2"   , hello2ILReduced)
          , ("hello4"   , hello2ILReduced)
          , ("writenum" , writeNumILReduced)
          , ("multiply" , multiplyILReduced)
          , ("readnum"  , readNumILReduced)
          , ("fact"     , factILReduced)
          , ("bottles"  , bottlesILReduced)
          , ("euclid"   , euclidILReduced)
          ] $ \(fileName , ilReduced) ->
      it fileName $ generateCode ilReduced `goldenShouldSafe` buildAbsolutePathToEtaFile ("generateCode" </> fileName)

  describe "naturalToDigitString" $
    forM_ [ (0      , "")
          , (1      , "t")
          , (10     , "to")
          , (11     , "ti")
          , (110    , "atn")
          , (111    , "ats")
          , (1110   , "otii")
          , (1111   , "otin")
          , (11110  , "iiant")
          , (11111  , "iiana")
          , (111110 , "sitsos")
          , (111111 , "sitsih")
          ] $ \(value , shown) ->
       it (show value) $ naturalToDigitString value `shouldSafe` shown
