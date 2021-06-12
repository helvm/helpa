module HelVM.HelPA.Assemblers.WSA.AssemblerSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.Assembler
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.CartesianProduct
import HelVM.Expectations

import HelVM.HelPA.Assembler.API
import HelVM.HelPA.Assembler.AssemblyOptions

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  let assembleLibFile = \ fileName options -> assembleFile SourcePath {dirPath = libDir , filePath = libDir </> fileName <.> ext} options
  let assembleAppFile = \ fileName options -> assembleFile SourcePath {dirPath = libDir , filePath = appDir </> fileName <.> ext} options

  describe "assembleLib" $ do
    forM_ ([ "io"
           , "memory"
           ] |><< manyOptionsWithName) $ \(fileName , name , options) -> do
      let assembleLib = assembleLibFile fileName options
      let minorPath = name </> fileName
      it minorPath $ do
        assembleLib `goldenShouldSafeReturn` buildAbsolutePathToWsFile ("assembleLib" </> minorPath)

  describe "assembleApp" $ do
    describe "original" $ do
      forM_ ([ "prim"
             ] |><< manyOptionsWithName) $ \(fileName , name , options) -> do
        let assembleApp = assembleAppFile fileName options
        let minorPath = name </> fileName
        it minorPath $ do
          assembleApp `goldenShouldSafeReturn` buildAbsolutePathToWsFile ("assembleApp" </> "original" </> minorPath)

    describe "from-eas" $ do
      forM_ ([ "true"
             , "hello"
             , "pip"
  --           , "pip2"
  --           , "reverse"
  --           , "function"
  --           , "add"
  --           , "writestr"
             , "hello2"
  --           , "hello3"
             , "hello4"
  ----           , "writenum"
  --           , "multiply"
  ----           , "readnum"
  --           , "fact"
             , "bottles"
  --           , "euclid"
              ] |><< manyOptionsWithName) $ \(fileName , name , options) -> do
        let assemble = assembleFile SourcePath {dirPath = libDir , filePath = wsaDir </> "from-eas" </> fileName <.> ext} options
        let minorPath = name </> fileName
        it minorPath $ do
          assemble `goldenShouldSafeReturn` buildAbsolutePathToWsFile ("assembleApp" </> "from-eas" </> minorPath)

--  describe "assembleFile" $ do
--    it "io"     $ do assembleLibFile "io"     visibleTokenTypeOptions `shouldSafeReturn` showTL ioTL
--    it "memory" $ do assembleLibFile "memory" visibleTokenTypeOptions `shouldSafeReturn` showTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   visibleTokenTypeOptions `shouldSafeReturn` showTL primTL
--
--    it "io"     $ do assembleLibFile "io"     allFalse `shouldSafeReturn` showTLAsWTL ioTL
--    it "memory" $ do assembleLibFile "memory" allFalse `shouldSafeReturn` showTLAsWTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   allFalse `shouldSafeReturn` showTLAsWTL primTL
--
--    it "io"     $ do assembleLibFile "io"     bothTokenTypeOptions `shouldSafeReturn` showTLAsBTL ioTL
--    it "memory" $ do assembleLibFile "memory" bothTokenTypeOptions `shouldSafeReturn` showTLAsBTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   bothTokenTypeOptions `shouldSafeReturn` showTLAsBTL primTL
