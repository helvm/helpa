module HelVM.HelPA.Assemblers.WSA.LinkerSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.Linker
import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.Expectations

import HelVM.HelPA.Assembler.API

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  let linkLibFile = \ fileName -> linkLib SourcePath {dirPath = libDir, filePath = fileName <.> ext}
  let linkAppFile = \ fileName -> linkApp SourcePath {dirPath = libDir, filePath = appDir </> fileName <.> ext}
  let linkAppFile1 = \ fileName -> linkApp SourcePath {dirPath = libDir, filePath = wsaDir </> "from-eas" </> fileName <.> ext}

  describe "linkLibFile" $ do
    forM_ [ "io"
          , "memory"
          ] $ \fileName -> do
      it fileName $ do
        linkLibFile fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("linkLibFile" </> fileName)

  describe "linkAppFile" $ do
    describe "original" $ do
      forM_ [ "prim"
            ] $ \fileName -> do
        it fileName $ do
          linkAppFile fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("linkAppFile" </> "original" </> fileName)

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
          linkAppFile1 fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("linkAppFile" </> "from-eas" </> fileName)

  describe "linkFile" $ do
    it "io"     $ do linkLibFile "io"     `shouldSafeReturn` ioIL
    it "memory" $ do linkLibFile "memory" `shouldSafeReturn` memoryIL
    it "prim"   $ do linkAppFile "prim"   `shouldSafeReturn` (primIL <> ioIL)
