module HelVM.HelPA.Assemblers.WSA.LinkerSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.FileUtil
import           HelVM.HelPA.Assemblers.WSA.Linker
import           HelVM.HelPA.Assemblers.WSA.TestData

import           HelVM.HelPA.Assembler.API

import           HelVM.Common.Util

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                          (Spec, describe, it)

spec :: Spec
spec = do
  let linkLibFile  fileName = linkLib SourcePath {dirPath = libDir , filePath = fileName <.> ext}
  let linkAppFile  fileName = linkApp SourcePath {dirPath = libDir , filePath = appDir </> fileName <.> ext}
  let linkAppFile1 fileName = linkApp SourcePath {dirPath = libDir , filePath = wsaDir </> "from-eas" </> fileName <.> ext}

  describe "linkLibFile" $ do
    forM_ [ "io"
          , "memory"
          ] $ \fileName -> do
      it fileName $ do
        showP <$> linkLibFile fileName `goldenShouldSafeExceptT` buildAbsolutePathToIlFile ("linkLibFile" </> fileName)

  describe "linkAppFile" $ do
    describe "original" $ do
      forM_ [ "prim"
            ] $ \fileName -> do
        it fileName $ do
          showP <$> linkAppFile fileName `goldenShouldSafeExceptT` buildAbsolutePathToIlFile ("linkAppFile" </> "original" </> fileName)

    describe "from-eas" $ do
      forM_ [ "true"
            , "hello"
            , "pip"
  --           , "pip2"
  --           , "reverse"
  --           , "function"
  --           , "add"
  --           , "writestr"
            , "hello2"
  --          , "hello3"
            , "hello4"
  ----          , "writenum"
  --          , "multiply"
  ----          , "readnum"
  --          , "fact"
            , "bottles"
 --           , "euclid"
            ] $ \ fileName -> do
        it fileName $ do
          showP <$> linkAppFile1 fileName `goldenShouldSafeExceptT` buildAbsolutePathToIlFile ("linkAppFile" </> "from-eas" </> fileName)

  describe "linkFile" $ do
    it "io"     $ do linkLibFile "io"     `shouldSafeExceptT` ioIL
    it "memory" $ do linkLibFile "memory" `shouldSafeExceptT` memoryIL
    it "prim"   $ do linkAppFile "prim"   `shouldSafeExceptT` (primIL <> ioIL)
