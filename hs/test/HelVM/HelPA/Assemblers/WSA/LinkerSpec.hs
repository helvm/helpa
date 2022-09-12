module HelVM.HelPA.Assemblers.WSA.LinkerSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.FileExtra
import           HelVM.HelPA.Assemblers.WSA.Linker
import           HelVM.HelPA.Assemblers.WSA.TestData

import           HelVM.HelIO.Extra
import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec = do
  let linkLibFile  fileName = linkLib SourcePath {dirPath = libDir , filePath = fileName <.> lang}
  let linkAppFile  fileName = linkApp SourcePath {dirPath = libDir , filePath = appDir </> fileName <.> lang}
  let linkAppFile1 fileName = linkApp SourcePath {dirPath = libDir , filePath = wsaDir </> "from-eas" </> fileName <.> lang}

  describe "linkLibFile" $
    forM_ [ "io"
          , "memory"
          ] $ \fileName ->
      it fileName $
        showP <$> linkLibFile fileName `goldenShouldControlT` buildAbsolutePathToIlFile ("linkLibFile" </> fileName)

  describe "linkAppFile" $ do
    describe "original" $
      forM_ [ "prim"
            ] $ \fileName ->
        it fileName $
          showP <$> linkAppFile fileName `goldenShouldControlT` buildAbsolutePathToIlFile ("linkAppFile" </> "original" </> fileName)

    describe "from-eas" $
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
            ] $ \ fileName ->
        it fileName $
          showP <$> linkAppFile1 fileName `goldenShouldControlT` buildAbsolutePathToIlFile ("linkAppFile" </> "from-eas" </> fileName)

  describe "linkFile" $ do
    it "io"     $ linkLibFile "io"     `shouldControlT` ioIL
    it "memory" $ linkLibFile "memory" `shouldControlT` memoryIL
    it "prim"   $ linkAppFile "prim"   `shouldControlT` (primIL <> ioIL)
