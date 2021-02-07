module HelVM.HelPA.Assemblers.WSA.AssemblerSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.Assembler
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.HelPA.Assemblers.CartesianProduct
import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.API
import HelVM.HelPA.Common.AssemblyOptions

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
        assembleLib `goldenShouldParseReturn` buildAbsolutePathToWsFile ("assembleLib" </> minorPath)

  describe "assembleApp" $ do
    describe "original" $ do
      forM_ ([ "prim"
             ] |><< manyOptionsWithName) $ \(fileName , name , options) -> do
        let assembleApp = assembleAppFile fileName options
        let minorPath = name </> fileName
        it minorPath $ do
          assembleApp `goldenShouldParseReturn` buildAbsolutePathToWsFile ("assembleApp" </> "original" </> minorPath)

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
          assemble `goldenShouldParseReturn` buildAbsolutePathToWsFile ("assembleApp" </> "from-eas" </> minorPath)

--  describe "assembleFile" $ do
--    it "io"     $ do assembleLibFile "io"     visibleTokenTypeOptions `shouldParseReturn` showTL ioTL
--    it "memory" $ do assembleLibFile "memory" visibleTokenTypeOptions `shouldParseReturn` showTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   visibleTokenTypeOptions `shouldParseReturn` showTL primTL
--
--    it "io"     $ do assembleLibFile "io"     allFalse `shouldParseReturn` showTLAsWTL ioTL
--    it "memory" $ do assembleLibFile "memory" allFalse `shouldParseReturn` showTLAsWTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   allFalse `shouldParseReturn` showTLAsWTL primTL
--
--    it "io"     $ do assembleLibFile "io"     bothTokenTypeOptions `shouldParseReturn` showTLAsBTL ioTL
--    it "memory" $ do assembleLibFile "memory" bothTokenTypeOptions `shouldParseReturn` showTLAsBTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   bothTokenTypeOptions `shouldParseReturn` showTLAsBTL primTL
