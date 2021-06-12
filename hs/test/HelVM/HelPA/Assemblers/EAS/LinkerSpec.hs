module HelVM.HelPA.Assemblers.EAS.LinkerSpec where

import HelVM.HelPA.Assemblers.EAS.Linker

import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil

import HelVM.Expectations

import HelVM.HelPA.Assembler.API

import Test.Hspec

spec :: Spec
spec = do
  describe "Files" $ do
    forM_ [ ("true"     , trueIL)
          , ("hello"    , helloIL)
          , ("pip"      , pipIL)
          , ("pip2"     , pip2IL)
          , ("reverse"  , reverseIL)
          , ("function" , functionIL)
          , ("writestr" , writeStrIL)
          , ("hello2"   , hello2ILLinked)
          , ("hello3"   , hello2ILLinked)
          , ("hello4"   , hello4ILLinked)
          , ("writenum" , writeNumIL)
          , ("multiply" , multiplyIL)
          , ("readnum"  , readNumIL)
          , ("fact"     , factILLinked)
          , ("bottles"  , bottlesILLinked)
          , ("euclid"   , euclidIL)
          ] $ \(fileName , il) -> do
      let linkFile = linkLib SourcePath {dirPath = "examples/eas/", filePath = buildEasFileName fileName}
      it fileName $ do linkFile `shouldSafeReturn` il
