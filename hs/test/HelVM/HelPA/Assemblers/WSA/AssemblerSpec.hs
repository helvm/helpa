module HelVM.HelPA.Assemblers.WSA.AssemblerSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.Assembler
import           HelVM.HelPA.Assemblers.WSA.AssemblyOptionsUtil
import           HelVM.HelPA.Assemblers.WSA.FileUtil

import           HelVM.HelPA.Assembler.API

import           HelVM.Common.NamedValue

import           HelVM.CartesianProduct
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                     (Spec, describe, it)

spec :: Spec
spec = do
  let assembleLibFile fileName = assembleFile SourcePath {dirPath = libDir , filePath = libDir </> fileName <.> ext}
  let assembleAppFile fileName = assembleFile SourcePath {dirPath = libDir , filePath = appDir </> fileName <.> ext}

  describe "assembleLib" $ do
    forM_ ([ "io"
           , "memory"
           ] |><| manyOptionsWithName) $ \(fileName , namedOptions) -> do
      let assembleLib = assembleLibFile fileName $ value namedOptions
      let minorPath = name namedOptions </> fileName
      it minorPath $ do
        assembleLib `goldenShouldSafeExceptT` buildAbsolutePathToWsFile ("assembleLib" </> minorPath)

  describe "assembleApp" $ do
    describe "original" $ do
      forM_ ([ "prim"
             ] |><| manyOptionsWithName) $ \(fileName , namedOptions) -> do
        let assembleApp = assembleAppFile fileName $ value namedOptions
        let minorPath = name namedOptions </> fileName
        it minorPath $ do
          assembleApp `goldenShouldSafeExceptT` buildAbsolutePathToWsFile ("assembleApp" </> "original" </> minorPath)

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
              ] |><| manyOptionsWithName) $ \(fileName , namedOptions) -> do
        let assemble = assembleFile SourcePath {dirPath = libDir , filePath = wsaDir </> "from-eas" </> fileName <.> ext} $ value namedOptions
        let minorPath = name namedOptions </> fileName
        it minorPath $ do
          assemble `goldenShouldSafeExceptT` buildAbsolutePathToWsFile ("assembleApp" </> "from-eas" </> minorPath)

--  describe "assembleFile" $ do
--    it "io"     $ do assembleLibFile "io"     visibleTokenTypeOptions `shouldSafeIO` showTL ioTL
--    it "memory" $ do assembleLibFile "memory" visibleTokenTypeOptions `shouldSafeIO` showTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   visibleTokenTypeOptions `shouldSafeIO` showTL primTL
--
--    it "io"     $ do assembleLibFile "io"     allFalse `shouldSafeIO` showTLAsWTL ioTL
--    it "memory" $ do assembleLibFile "memory" allFalse `shouldSafeIO` showTLAsWTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   allFalse `shouldSafeIO` showTLAsWTL primTL
--
--    it "io"     $ do assembleLibFile "io"     bothTokenTypeOptions `shouldSafeIO` showTLAsBTL ioTL
--    it "memory" $ do assembleLibFile "memory" bothTokenTypeOptions `shouldSafeIO` showTLAsBTL memoryTL
--    it "prim"   $ do assembleAppFile "prim"   bothTokenTypeOptions `shouldSafeIO` showTLAsBTL primTL
